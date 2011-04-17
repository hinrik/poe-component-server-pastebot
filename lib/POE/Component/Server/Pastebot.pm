package POE::Component::Server::Pastebot;

use strict;
use warnings FATAL => 'all';

use Carp;
use File::Path 'make_path';
use File::ShareDir 'dist_dir';
use File::Spec::Functions 'catfile';
use HTTP::Negotiate;
use HTTP::Response;
use Perl::Tidy;
use POE;
use POE::Filter::HTTPD;
use POE::Component::Server::TCP;
use POE::Component::Server::Pastebot::WebUtil qw(
    static_response parse_content parse_cookie dump_content html_encode
    is_true cookie);
use Socket;
use Storable;

use constant {
    PASTE_TIME    => 0,
    PASTE_SUMMARY => 1,
    PASTE_ID      => 2,
    PASTE_PLACE   => 3,
    PASTE_HOST    => 4,
};

sub new {
    my ($package, %args) = @_;
    my $self = bless \%args, $package;

    croak "No paste_dir supplied" if !defined $self->{paste_dir};
    croak "No iname supplied"     if !defined $self->{iname};
    $self->{count}   = 0          if !defined $self->{count};
    $self->{check}   = 3600       if !defined $self->{check};
    $self->{expires} = 864000     if !defined $self->{expires};
    $self->{port}    = 8888       if !defined $self->{port};
    $self->{iface}   = '0.0.0.0'  if !defined $self->{iface};
    $self->{registered} = {};

    if (!defined $self->{static_dir}) {
        $self->{static_dir} = dist_dir('POE-Component-Server-Pastebot');
    }

    if (!-d $self->{paste_dir}) {
        local $@ = undef;
        eval { make_path($self->{paste_dir}) };
        chomp $@;
        croak "Couldn't create directory $self->{paste_dir}: $@\n" if $@;
    }

    if (defined $self->{template_class}) {
        my $class = $self->{template_class};
        eval "require $class";
        chomp $@;
        croak "Unable to load template class '$class': $@" if $@; 

        $self->{template} = $class->new()
            or croak "Unable to instantiate template object.\n";

    }
    else {
        require POE::Component::Server::Pastebot::TextTemplate;
        $self->{template} = POE::Component::Server::Pastebot::TextTemplate->new()
            or croak "Unable to instantiate default template object.\n";
    }

    POE::Session->create(
        object_states => [
            $self => [qw(
                _start
                check_expired
                sig_die
                _shutdown
            )],
        ],
    );

    return $self;
}

sub register {
    my ($self, $session_id) = @_;
    return if $self->{registered}{$session_id};
    $self->{registered}{$session_id} = 1;
    $poe_kernel->refcount_increment($session_id, __PACKAGE__);
    return;
}

sub unregister {
    my ($self, $session_id) = @_;
    return if !$self->{registered}{$session_id};
    delete $self->{registered}{$session_id};
    $poe_kernel->refcount_decrement($session_id, __PACKAGE__);
    return;
}

sub _page_footer {
    return "<div align=right><font size='-1'>"
        ."<a href='http://sf.net/projects/pastebot/'>Pastebot</a>"
        ." is powered by "
        ."<a href='http://poe.perl.org/'>POE</a>.</font></div>";
}

sub _start {
    my ($kernel, $self, $session) = @_[KERNEL, OBJECT, SESSION];
    $self->{session_id} = $session->ID;

    $kernel->sig(DIE => 'sig_die');

    my $index = catfile($self->{paste_dir}, 'Index');
    if (-e $index) {
        $self->{paste_cache} = retrieve($index);
        $self->{id_sequence} = (sort keys %{ $self->{paste_cache} })[-1];
    }

    #my $ignore = catfile($self->{paste_dir}, 'Ignorelist');
    #if (-e $ignore) {
    #    $self->{ignores} = retrieve($ignore);
    #}

    $self->{server_id} = POE::Component::Server::TCP->new(
        Port     => $self->{port},
        Address  => $self->{iface},
        Acceptor => sub {
            POE::Session->create(
                object_states => [
                    $self => [qw(httpd_flushed httpd_queried httpd_failed)],
                    $self => {_start => 'httpd_started'},
                ],
                args => [@_[ARG0..ARG2]],
            )
        },
    );

    $kernel->delay(check_expired => $self->{check});
    return;
}

sub _shutdown {
    my $kernel = $_[KERNEL];
    $kernel->delay('check_expired');
    return;
}

sub shutdown {
    my ($self) = @_;
    $self->unregister($_) for keys %{ $self->{registered} };
    $poe_kernel->post($self->{server_id}, 'shutdown');
    $poe_kernel->post($self->{session_id}, '_shutdown');
    return;
}

sub check_expired {
    my ($self) = $_[OBJECT];

    for (keys %{ $self->{paste_cache} }) {
        next unless (
        (time - $self->{paste_cache}{$_}->[PASTE_TIME]) > $self->{expires}
        );
        $self->delete_paste_by_id($_);
    }

    return;
}

sub sig_die {
    my ($kernel, $self, $ex) = @_[KERNEL, OBJECT, ARG1];
    chomp $ex->{error_str};

    my @errors = (
        "Event $ex->{event} in session ".$ex->{dest_session}->ID." raised exception:",
        "    $ex->{error_str}",
    );

    warn "$_\n" for @errors;
    $kernel->sig_handled();
    return;
}

sub _delete_paste_by_id {
    my ($self, $id) = @_;
    delete $self->{paste_cache}{$id};
    my $paste_file = catfile($self->{paste_dir}, $id);
    unlink $paste_file or warn "Problem removing paste $id: $!\n";
    store($self->{paste_cache}, catfile($self->{paste_dir}, 'Index'));
    return;
}

sub add_place {
    my ($self, $place) = @_;
    $self->{places}{$place} = 1;
    return;
}

sub remove_place {
    my ($self, $place) = @_;
    delete $self->{places}{$place};
    return;
}

sub httpd_started {
    my ($heap, $socket, $remote_address, $remote_port) = @_[HEAP, ARG0..$#_];

    $heap->{remote_addr} = inet_ntoa($remote_address);
    $heap->{remote_port} = $remote_port;
    $heap->{wheel} = POE::Wheel::ReadWrite->new(
        Handle       => $socket,
        Filter       => POE::Filter::HTTPD->new(),
        InputEvent   => 'httpd_queried',
        FlushedEvent => 'httpd_flushed',
        ErrorEvent   => 'httpd_failed',
    );
    return;
}

sub httpd_flushed {
    delete $_[HEAP]->{wheel};
    return;
}

sub httpd_failed {
    my ($heap, $session, $operation, $errnum, $errstr)
        = @_[HEAP, SESSION, ARG0..ARG2];
    warn 'Session '.$session->ID."got $operation error: $errnum: $errstr\n";
    delete $heap->{wheel};
    return;
}

sub httpd_queried {
    my ($kernel, $self, $heap, $request) = @_[KERNEL, OBJECT, HEAP, ARG0];

    ### Log the request.

    # Space-separated list:
    # Remote address (client address)
    # -
    # -
    # [GMT date in brackets: DD/Mon/CCYY:HH:MM:SS -0000]
    # "GET url HTTP/x.y"  <-- in quotes
    # response code
    # response size
    # referer
    # user-agent string

    ### Responded with an error.  Send it directly.

    if ($request->isa("HTTP::Response")) {
        $heap->{wheel}->put($request);
        return;
    }

    ### These requests don't require authentication.

    my $url = $request->url() . '';

    # strip multiple // to prevent errors
    $url =~ s,//+,/,;

    # simple url decode
    $url =~ s,%([[:xdigit:]]{2}),chr hex $1,eg;

    ### Fetch the highlighted style sheet.

    if ($url eq '/style') {
        my $response = static_response(
            $self->{template}, "$self->{static_dir}/highlights.css", { }
        );
        $heap->{wheel}->put( $response );
        return;
    }

    ### Fetch some kind of data.

    if ($url =~ m{^/static/(.+?)\s*$}) {
        # TODO - Better path support?
        my $filename = $1;
        $filename =~ s{/\.+}{/}g;  # Remove ., .., ..., etc.
        $filename =~ s{/+}{/}g;    # Combine // into /
        $filename = "$self->{static_dir}/$filename";

        my ($code, $type, $content);

        if (-e $filename) {
            if (open my $fh, '<', $filename) {
                $code = 200;
                local $/ = undef;
                $content = <$fh>;
                close $fh;

                # TODO - Better type support.
                if ($filename =~ /\.(gif|jpe?g|png)$/i) {
                    $type = lc($1);
                    $type = "jpeg" if $type eq "jpg";
                    $type = "image/$1";
                }
            }
            else {
                $code = 500;
                $type = "text/html";
                $content = (
                    "<html><head><title>File Error</title></head>" .
                    "<body>Error opening $filename: $!</body></html>"
                );
            }
        }
        else {
            $code = 404;
            $type = "text/html";
            $content = (
                "<html><head><title>404 File Not Found</title></head>" .
                "<body>File $filename does not exist.</body></html>"
            );
        }

        my $response = HTTP::Response->new($code);
        $response->push_header('Content-type', $type);
        $response->content($content);
        $heap->{wheel}->put( $response );
        return;
    }

    ### Store paste.

    if ($url =~ m,/paste$,) {
        my $content = parse_content($request->content());

        if (defined $content->{paste} and length $content->{paste}) {
            my $place = $content->{place};
            defined $place or $place = "";

            my $remote_addr = $heap->{remote_addr};
            if ($self->{proxy} && $remote_addr eq $self->{proxy}) {
                # apache sets the X-Forwarded-For header to a list of the
                # IP addresses that were forwarded from/to
                my $forwarded = $request->headers->header('X-Forwarded-For');
                if ($forwarded) {
                    ($remote_addr) = $forwarded =~ /([^,\s]+)$/;
                }
                # else must be local?
            }

            my $error = "";
            #if (length $place) {
            #    # See if it matches.
            #    if (is_ignored($self->{isrv}, $channel, $remote_addr)) {
            #        $error = (
            #            "<p><b><font size='+1' color='#800000'>" .
            #            "Your IP address has been blocked from pasting to $channel." .
            #            "</font></b></p>"
            #        );
            #        $channel = "";
            #    }
            #}

            # Goes as a separate block.
            if (length $place && !exists $self->{places}{$place}) {
                $error = (
                    "<p><b><font size='+1' color='#800000'>" .
                    "I'm not in $place." .
                    "</font></b></p>"
                );
                $place = "";
            }

            my $nick = $content->{nick};
            $nick = "" unless defined $nick;
            $nick =~ tr[\x00-\x1F\x7F][ ]s;
            $nick =~ s/\s+/ /g;
            $nick =~ s/^\s+//;
            $nick =~ s/\s+$//;
            $nick = html_encode($nick);

            # <CanyonMan> how about adding a form field with a "Subject"
            # line ?

            my $summary = $content->{summary};
            $summary = "" unless defined $summary;
            $summary =~ tr[\x00-\x1F\x7F][ ]s;
            $summary =~ s/\s+/ /g;
            $summary =~ s/^\s+//;
            $summary =~ s/\s+$//;

            # <TorgoX> [...] in the absence of anything in the subject, it
            # falls back to [the first 30 characters of what's pasted]

            my $paste = $content->{paste};
            unless (length($summary)) {
                $summary = $paste;
                $summary =~ s/\s+/ /g;
                $summary =~ s/^\s+//;
                $summary = substr($summary, 0, 30);
                $summary =~ s/\s+$//;
            }

            $summary = "something" unless length $summary;
            my $html_summary = html_encode($summary);

            my $id = $self->store_paste(
                $nick, $html_summary, $paste,
                $place, $remote_addr
            );
            my $paste_link = (
                $self->{iname} .
                (
                    ($self->{iname} =~ m,/$,)
                    ? $id
                    : "/$id"
                )
            );

            # show number of lines in paste in announcement
            my $paste_lines = 0;
            $paste_lines++ for $paste =~ m/^.*$/mg;

            $paste = format_paste($paste, 0, 0, 0, 0);

            my $response = static_response(
                $self->{template},
                "$self->{static_dir}/paste-answer.html",
                {
                    paste_id   => $id,
                    error      => $error,
                    paste_link => $paste_link,
                    nick       => (length $nick ? $nick : 'Someone'),
                    summary    => $summary,
                    address    => $remote_addr,
                    paste      => $paste,
                    footer     => _page_footer(),
                }
            );

            if ($place && keys %{ $self->{registered} }) {
                for my $session_id (keys %{ $self->{registered} }) {
                    $kernel->post(
                        $session_id,
                        'pastebot_pasted',
                        $place,
                        $nick,
                        $remote_addr,
                        $summary,
                        $paste_lines,
                        $paste_link,
                    );
                }
            }

            $heap->{wheel}->put($response);
            return;
        }

    # Error goes here.
    }

    ### Fetch paste.

    if ($url =~ m{^/(\d+)(?:\?(.*?)\s*)?$}) {
        my ($num, $params) = ($1, $2);
        my ($nick, $summary, $paste) = $self->fetch_paste($num);

        if (defined $paste) {
            my $cookie = parse_cookie($request->headers->header('Cookie'));
            my $query  = parse_content($params);

          ### Make the paste pretty.

            my $ln   = exists $query ->{ln}   ? is_true($query ->{ln})   :
                       exists $cookie->{ln}   ? is_true($cookie->{ln})   : 0;
            my $tidy = exists $query ->{tidy} ? is_true($query ->{tidy}) :
                       exists $cookie->{tidy} ? is_true($cookie->{tidy}) : 0;
            my $hl   = exists $query ->{hl}   ? is_true($query ->{hl})   :
                       exists $cookie->{hl}   ? is_true($cookie->{hl})   : 0;
            my $tx   = exists $query ->{tx}   ? is_true($query ->{tx})   :
                       exists $cookie->{tx}   ? is_true($cookie->{tx})   : 0;
            my $wr   = exists $query ->{wr}   ? is_true($query ->{wr})   :
                       exists $cookie->{wr}   ? is_true($cookie->{wr})   : 0;
            my $store = is_true($query->{store});

            my $variants = [
                ['html', 1.000, 'text/html',  undef, 'us-ascii', 'en', undef],
                ['text', 0.950, 'text/plain', undef, 'us-ascii', 'en', undef],
            ];
            my $choice = choose($variants, $request);
            $tx = 1 if $choice && $choice eq 'text';

            $paste = format_paste($paste, $ln, $tidy, $hl, $wr) unless $tx;

            # Spew the paste.

            my $response;
            if ($tx) {
                $response = HTTP::Response->new(200);
                $response->push_header( 'Content-type', 'text/plain' );
                $response->content($paste);
            }
            else {
                $response = static_response(
                    $self->{template},
                    "$self->{static_dir}/paste-lookup.html",
                    {
                        bot_name => $self->{name},
                        paste_id => $num,
                        nick     => $nick,
                        summary  => $summary,
                        paste    => $paste,
                        footer   => _page_footer(),
                        tidy     => ( $tidy ? "checked" : "" ),
                        hl       => ( $hl   ? "checked" : "" ),
                        ln       => ( $ln   ? "checked" : "" ),
                        tx       => ( $tx   ? "checked" : "" ),
                        wr       => ( $wr   ? "checked" : "" ),
                    }
                );

                if ($store) {
                    $response->push_header('Set-Cookie' => cookie(tidy => $tidy, $request));
                    $response->push_header('Set-Cookie' => cookie(hl   => $hl, $request));
                    $response->push_header('Set-Cookie' => cookie(wr   => $wr, $request));
                    $response->push_header('Set-Cookie' => cookie(ln   => $ln, $request));
                }
            }

            $heap->{wheel}->put( $response );
            return;
        }

        my $response = HTTP::Response->new(404);
        $response->push_header( 'Content-type', 'text/html' );
        $response->content(
            "<html>" .
            "<head><title>Paste Not Found</title></head>" .
            "<body><p>Paste not found.</p></body>" .
            "</html>"
        );
        $heap->{wheel}->put( $response );
        return;
    }

    ### Root page.

    # 2003-12-22 - RC - Added _ and - as legal characters for place
    # names.  What else?
    #&!+%
    if (my ($prefplace) = $url =~ m{^/([#&!+%]\S+)?}) {
        $prefplace = '' if !defined $prefplace;
        # Dynamically build the channel options from the configuration
        # file's list.
        my @places = keys %{ $self->{places} };
        unshift @places, '';

        @places = map {
            qq(<option value="$_")
            . ($_ eq $prefplace ? ' selected' : '')
            . '>'
            . ($_ eq '' ? '(none)' : $_)
            . '</option>'
        } sort @places;

        # Build content.

        my $iname = $self->{iname};
        $iname .= '/' unless $iname =~ m#/$#;
        my $response = static_response(
            $self->{template},
            "$self->{static_dir}/paste-form.html",
            {
                bot_name => $self->{name},
                places   => "@places",
                footer   => _page_footer(),
                iname    => $iname,
            }
        );
        $heap->{wheel}->put($response);
        return;
    }

    ### Default handler dumps everything it can about the request.

    my $response = HTTP::Response->new( 200 );
    $response->push_header( 'Content-type', 'text/html' );

    # Many of the headers dumped here are undef.  We turn off warnings
    # here so the program doesn't constantly squeal.

    #local $^W = 0;

    $response->content(
        "<html><head><title>Strange Request Dump</title></head>" .
        "<body>" .
        "<p>" .
        "Your request was strange.  " .
        "Here is everything I could figure out about it:" .
        "</p>" .
        "<table border=1>" .
        join(
            "",
            map {
                "<tr><td><header></td><td>" . $request->$_() . "</td></tr>"
            } qw(
                authorization authorization_basic content_encoding
                content_language content_length content_type content date
                expires from if_modified_since if_unmodified_since
                last_modified method protocol proxy_authorization
                proxy_authorization_basic referer server title url user_agent
                www_authenticate
            )
        ) .
        join(
        "",
        map {
            "<tr><td><header></td><td>" . $request->header($_) . "</td></tr>"
        } qw(
            Accept Connection Host
            username opaque stale algorithm realm uri qop auth nonce
            cnonce nc response
        )
        ) .
        "</table>" .
        dump_content($request->content()) .
        "<p>Request as string=" . $request->as_string() . "</p>" .
        "</body></html>"
    );

    # A little debugging here.
    if ($self->{debug}) {
        my $request_as_string = $request->as_string();
        warn unpack('H*', $request_as_string), "\n";
        warn "Request has CR.\n" if $request_as_string =~ /\x0D/;
        warn "Request has LF.\n" if $request_as_string =~ /\x0A/;
    }

    $heap->{wheel}->put( $response );
    return;
}

sub format_paste {
    my ($paste, $line_nums, $tidied, $highlighted, $wrapped) = @_;

    if ($tidied) {
        my $tidy_version = "";
        eval {
            Perl::Tidy::perltidy(
                source      => \$paste,
                destination => \$tidy_version,
                argv        => [ '-q', '-nanl', '-fnl' ],
            );
        };
        if ($@) {
            $paste = "Could not tidy this paste (try turning tidying off): $@";
        }
        else {
            $paste = $tidy_version;
        }
    }

      ### If the code is to be highlighted, then highlight it.

    if ($highlighted) {
        my @html_args = qw( -q -html -pre );
        push @html_args, "-nnn" if $line_nums;

        $highlighted = "";
        eval {
            Perl::Tidy::perltidy(
                source      => \$paste,
                destination => \$highlighted,
                argv        => \@html_args,
            );
        };
        if ($@) {
            $highlighted =
                "Could not highlight the paste (try turning highlighting off): $@";
        }
        return $highlighted;
    }

    ### Code's not highlighted.  HTML escaping time.  Forgive me.

    # Prepend line numbers to each line.

    if ($line_nums) {
        my $total_lines = 0;
        $total_lines++ while ($paste =~ m/^/gm);
        my $line_number_width = length($total_lines);
        $line_number_width = 4 if $line_number_width < 4;  # To match Perl::Tidy.

        my $line_number = 0;
        while ($paste =~ m/^/gm) {
            my $pos = pos($paste);
            $line_number++;
            my $new = sprintf "\%${line_number_width}d ", $line_number;
            substr($paste, pos($paste), 0, $new);
            pos($paste) = $pos + 1;
        }
    }

    $paste = html_encode($paste);

    # Normalize newlines.  Translate whichever format to just \n, and
    # limit the number of consecutive newlines to two.

    $paste =~ s/(\x0d\x0a?|\x0a\x0d?)/\n/g;
    $paste =~ s/\n\n+/\n\n/;

    # Buhbye.

    unless ($wrapped) {
        substr $paste, 0, 0, '<pre>';
        $paste .= '</pre>';
    }

    return $paste;
}

sub check_paste_count {
    my ($self) = @_;
    return if (scalar(keys %{ $self->{paste_cache} }) < $self->{count});
    my $oldest = (
        sort {
            $self->{paste_cache}{$a}->[PASTE_TIME] > $self->{paste_cache}{$b}->[PASTE_TIME]
        } keys %{ $self->{paste_cache} }
    )[0];
    $self->_delete_paste_by_id($oldest) if defined $oldest;
    return;
}

sub store_paste {
    my ($self, $id, $summary, $paste, $place, $ipaddress) = @_; 
    $self->check_paste_count();

    my $new_id = ++$self->{id_sequence};
    $self->{paste_cache}{$new_id} = [
        time(),       # PASTE_TIME
        $summary,     # PASTE_SUMMARY
        $id,          # PASTE_ID
        $place,       # PASTE_PLACE
        $ipaddress,   # PASTE_HOST
    ];

    my $dir = $self->{paste_dir};

    store($self->{paste_cache}, catfile($dir, 'Index'));

    my $paste_file = catfile($dir, $new_id);
    open my $fh, '>', $paste_file or warn "I cannot store paste $new_id: $!";
    binmode $fh;
    print $fh $paste;
    close $fh;

    return $new_id;
}

sub fetch_paste {
    my ($self, $id) = @_;
    my $paste = $self->{paste_cache}{$id};
    return(undef, undef, undef) unless defined $paste;

    my $dir = $self->{paste_dir};

    my $fh;
    unless(open $fh, '<', catfile($dir, $id)) {
        warn "Error opening paste $id: $!";
        return(undef, undef, undef);
    }
    local $/ = undef;
    my $content = <$fh>;
    close $fh;

    return $paste->[PASTE_ID], $paste->[PASTE_SUMMARY], $content;
}

1;

=encoding utf8

=head1 NAME

POE::Component::Server::Pastebot - A paste webservice

=head1 SYNOPSIS

 use POE::Component::Server::Pastebot;

 sub _start {
     my ($self, $session) = @_[OBJECT, SESSION];

     $self->{pastebot} = POE::Component::Server::Pastebot->new(
         paste_dir => '/tmp/pastes',
         iname     => 'http://server:8080',
     );
     $self->{pastebot}->register($session->ID);
 }

 sub pastebot_pasted {
     my ($self, $place, $nick, $address, $summary, $lines, $link)
         = @_[OBJECT, ARG0..$#_];

     # handle the paste
 }

=head1 DESCRIPTION

This pastebot runs a webserver where people can paste text. Whenever someone
does so, you will receive an event with details of the paste.

=head1 METHODS

=head2 C<new>

Creates a new POE::Component::Server::Pastebot object. Takes the following
optional arguments:

B<'paste_dir'> (required), where the pastes will be stored. This directory
will be created if it doesn't exist.

B<'iname'> (required), what your server is known as on the internet (e.g.
'http://server.com').

B<'port'> (optional), the port where the webservice will run. Defaults to
8888.

B<'iface'> (optional), the interface on which the webservice will listen.
Defaults to '0.0.0.0'.

B<'proxy'> (optional), if you are running this behind a proxy, set the proxy
IP address here.

B<'static_dir'> (optional), the directory that contains static files that will
be used by the pastebot's built-in web server. Templates, graphics, and style
sheets for the various served pages go here. It is essentially the Pastebot
server's document root. Defaults to the directory which came with the
module.

B<'template'> (optional), the class that implements the templating for
generated pages. The class must define specific methods, see
L<POE::Component::Server::Pastebot::TextTemplate|POE::Component::Server::Pastebot::TextTemplate>
for which methods and return values are expected.

B<'check'> (optional), how often to check for expired pastes. The value is
the number of seconds between each check. 3600 is an hour, 86400 is a day.
Default is 3600.

B<'count'> (optional), a limit on the number of pastes a pastebot's database
may hold, regardless whether they're too young to expire. This limit will be
disabled by setting it to zero (which is the default).

B<'expire'> (optional), the maximum limit for the age of pastes to retain.
This limit may be disabled if it is set to zero. If set, however, pastes that
are older than this value in seconds will be purged during a periodic
expiration check. The default is 864000.

=head2 C<register>

Registers a session for receiving paste events. Takes one argument, a POE
session id.

=head2 C<unregister>

Unregisters a session. Takes one argument, a POE session id.

=head2 C<add_place>

Adds a place which pasters can select when pasting. Takes one argument, the
name of the place.

=head2 C<add_place>

Removes a place. Takes one argument, the name of the place.

=head2 C<shutdown>

Takes no arguments. Unregisters all sessions and shuts down the web server.

=head1 OUTPUT

=head2 C<pastebot_pasted>

Sent to registered sessions after a new paste has been created. Has the
following arguments:

=over 4

=item * C<ARG0>: the place (if any) selected by the user

=item * C<ARG1>: the nickname (if any) of the user

=item * C<ARG2>: the IP address of the user

=item * C<ARG3>: a summary of the paste

=item * C<ARG4>: the number of lines in the paste

=item * C<ARG4>: a link to the paste

=back

=head1 AUTHOR

Rocco Caputo

Hinrik E<Ouml>rn SigurE<eth>sson, hinrik.sig@gmail.com

=head1 LICENSE AND COPYRIGHT

Copyright 2010 Rocco Caputo, Hinrik E<Ouml>rn SigurE<eth>sson

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
