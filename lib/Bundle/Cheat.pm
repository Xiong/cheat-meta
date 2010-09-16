package Bundle::Cheat;

use 5.008008;
use strict;
use warnings;

use version 0.77; our $VERSION = qv('0.0.1');

## END MODULE
return 1;   # Abort execution; fool syntax highlighting for following code.
die 'Failed to abort. This line should never execute; please contact author.';

#============================================================================#
#=========# ALL CHEAT MODULES

use Test::Cheat;

#============================================================================#
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

=head1 FUNCTIONS/METHODS

None. This module does nothing at all; you shouldn't even try to C<use()> it,
although it's harmless. Same goes for all *::Cheat modules. 

=head1 INTERFACE 

Open the module (the *.pm file itself) in your editor. 
Copy out whatever you like. 

=head1 DIAGNOSTICS

=over

=item Failed to abort. This line should never execute; please contact author.

This module contains executable code so that your editor will use
its syntax highlighting feature, if any. But it would be madness to execute it
all. Of the various strategies for suppressing execution of the cheat sheet, 
the one chosen is to C<return 1;> unconditionally. I can't imagine how this 
can fail; but if it does, I hope we will C<die()> before going further. 

=back

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
