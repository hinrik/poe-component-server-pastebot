use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;

use_ok('POE::Component::Server::Pastebot::WebUtil');
use_ok('POE::Component::Server::Pastebot::TextTemplate');
use_ok('POE::Component::Server::Pastebot');
