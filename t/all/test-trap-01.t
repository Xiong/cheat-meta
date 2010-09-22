#!/run/bin/perl
#       test-trap-01.t
#       = Copyright 2010 Xiong Changnian <xiong@cpan.org>    =
#       = Free Software = Artistic License 2.0 = NO WARRANTY =
#~ {
#~     package Acme::Teddy;
#~     sub one {1};
#~     sub dies {die'thrown'};
#~     sub prints {print'hello'};
#~ }
use 5.010001;
use strict;
use warnings;

#~ use Readonly;
#~ use feature qw(switch say state);
#~ use Perl6::Junction qw( all any none one );
#~ use List::Util qw(first max maxstr min minstr reduce shuffle sum);
#~ use File::Spec;
#~ use File::Spec::Functions qw(
#~     catdir
#~     catfile
#~     catpath
#~     splitpath
#~     curdir
#~     rootdir
#~     updir
#~     canonpath
#~ );
#~ use Cwd;
use Devel::Comments '###', '####';

#

#~ use Acme::Teddy;

my $name    = 'You know my name.';

use Test::More;
use Test::Trap qw( :scalar );
#~ use Test::Trap qw( :scalar :raw );
#~ use Test::Trap qw( :raw :scalar );
trap{
    # Your code under test here
    print   'hello';
    warn    'oopsy';
    warn    'floop';
#~     die;
#~     return  42;
#~     42;
    my $obj     = bless({},'Acme::Teddy');
    return $obj;
};
#~ use Devel::Comments '###';
#~ ### $trap
#~ no  Devel::Comments;
$trap->diag_all;

my $returns     = $trap->return;
### $returns

#~ my $leaveby     = $trap->leaveby;
#~ ### $leaveby

my $exception     = $trap->die;
### $exception

#~ say '# return: ', $trap->return(0);
plan tests  => 1;
#~ $trap->return_ok    ( 0,     $name );
#~ $trap->return_isa_ok    ( 0, 'Acme::Teddy', 'Acme::Teddy' );
$trap->return_isa_ok    ( 0, 'Acme::Teddy', 'returned object' );
#~ $trap->return_isa_ok    ( 0, 'Foo::Bar', 'Acme::Teddy' );
#~ $trap->stdout_like  ( qr/o/, $name );
#~ $trap->die_unlike( qr/hell|damn/, 'tried to print a word to screen');

__END__
