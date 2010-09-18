package Bundle::Cheat;

use 5.008008;
use strict;
use warnings;

use version 0.77; our $VERSION = qv('0.0.1');

#============================================================================#

sub _kids {(

    q{    Bundle::Cheat::Test         },
#~     q{    Bundle::Cheat::Util         },

)};


#============================================================================#
## END MODULE 
1;
__END__

=head1 NAME

Bundle::Cheat - Copy-and-paste usage lines for lazy coders

=head1 VERSION

This document describes Bundle::Cheat version 0.0.1

=head1 SYNOPSIS

    $ cat Bundle/Cheat.pm

=head1 DESCRIPTION

I<If a thing is worth having, it's worth cheating for.> 
--W. C. Fields

This is a collection of "cheat sheets": highly compressed, abbreviated 
documentation for various modules. Each module within the bundle covers a 
top-level namespace or a set of otherwise closely-related modules. 

For each module, a paragraph is given, generally: 

    Some::Module            # Short description
        qw( various exportable symbols if any );
        routine( $placeholder, @arguments );
        $context    = function( @arguments);
        $object->method();

You should be able to copy and paste this into your own code, 
delete what you don't need, and be on your way. 

=head1 CHEATS

=over

=item * L<Bundle::Cheat::Test>

=back

=head1 SEE ALSO

=over

=item * L<perlcheat>

=back

=head1 FUNCTIONS/METHODS

None. This module does nothing at all. 
You shouldn't even try to C<use()> it,
although it's harmless to do so. 

Other *::Cheat modules will C<die()> if used. 

=head1 INTERFACE 

Open the module (the *.pm file itself) in your editor. 
Copy out whatever you like. 

Yes, there could be many clever routines to print contents. Use your editor. 

=head1 DIAGNOSTICS

None. 

=head1 CONFIGURATION AND ENVIRONMENT

Bundle::Cheat requires no configuration files or environment variables.

=head1 DEPENDENCIES

No dependencies. 

=head1 INCOMPATIBILITIES

None.

=head1 BUGS AND LIMITATIONS

No cheat sheet will teach you anything. It's only a reminder. You B<must> 
consult each module's own full documentation I<at least> before using it. 
I hope. 

This module does not contain magic to squirt code into your module. 
Copy and paste. 

=head1 THANKS

=over

=item *

To about 8500 authors who have uploaded about 85,000 modules to the CPAN. 

=back

=head1 AUTHOR

Xiong Changnian  C<< <xiong@cpan.org> >>

=head1 LICENSE

Copyright (C) 2010 Xiong Changnian C<< <xiong@cpan.org> >>

This library and its contents are released under Artistic License 2.0:

L<http://www.opensource.org/licenses/artistic-license-2.0.php>

=cut
