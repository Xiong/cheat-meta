#!/run/bin/perl
#       cheat-build.pl
#       = Copyright 2010 Xiong Changnian <xiong@cpan.org> =
#       = Free Software = Artistic License 2.0 = NO WARRANTY =

use 5.10.1;
use strict;
use warnings;

#~ use Readonly;
#~ use feature qw(switch say state);
#~ use Perl6::Junction qw( all any none one );
#~ use List::Util qw(first max maxstr min minstr reduce shuffle sum);
#~ use File::Spec;
use File::Spec::Functions qw(
    catdir
    catfile
    catpath
    splitpath
    curdir
    rootdir
    updir
    canonpath
);
use Cwd;

#~ use Devel::Comments '###';
use Devel::Comments '####';

use Template;

## use

# File name, path stuff
my $base_dirabs         = getcwd();
my $lib_dirrel          = q{lib/Cheat};
my $tmpl_dirrel         = q{file};

my $tmpl_file           = q{sheet.tt2};
my $tmpl_fn_rel         = catfile ( $tmpl_dirrel, $tmpl_file );
my $raw_ext             = q{.perl};
my $out_ext             = q{.pod};

my $pod_indent          = q{ } x 4;

## pseudo-globals

# Always do work in one place
chdir $base_dirabs
    or die "$0: Failed to chdir $base_dirabs.", $!;

# Get command-line arguments
my @files_todo          = @ARGV;

# For each file...
for my $infile (@files_todo) {
    make_pod($infile);
};

{ # metro make_pod 

# Shared variables...
my $in_fh           ;
my @outlines        ;
my $symbols         = {
    package             => q{Cheat::},      # default ignored
    package_path        => q{Cheat/},       # default ignored
    tagline             => q{},
    quote               => q{},
    quote_cite          => q{},
    cheats              => [],
    outlines            => [],
};

    sub make_pod {
        my $infile          = $_[0];
        my $outfile         ;
        my $tt              = Template->new;
        
        # Process $infile line-by-line
        open $in_fh, '<', $infile
            or die "$0: Failed to open $infile to read.", $!;
        while ( my $in_line = <$in_fh> ) {
            given ($in_line) {
                when (/^#=package /)    { put_package($_) }
                when (/^#=tagline /)    { put_tagline($_) }
                when (/^#=quote/)       { put_quote  ($_) }
                when (/^#=/)            { put_pod    ($_) }
                when (/^[\s]*$/)        { put_blank  ($_) }
                default                 { put_code   ($_) }
            };
        };
        close $in_fh
            or die "$0: Failed to close $infile.", $!;
        
        $symbols->{outlines}    = \@outlines;
        
        #### $infile
        
        # Put the output somewhere
        $infile =~ s/$raw_ext$/$out_ext/;
        $outfile    = $infile;
        
        #### $outfile
        #### @outlines
        
        # Pass the stuff to Template, which opens, reads, and writes
        $tt->process( $tmpl_fn_rel, $symbols, $outfile )
            or do {
                warn $tt->error;
                die "$0: Failed to execute template process method.", $!;
            };
        
        return 1;
        
    }; ## make_pod
        
    # Helper subs...
    
    sub put_package {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=package //;
        $symbols->{package}         = $in_line;
        $symbols->{package_path}    = ( $in_line =~ s|::|/| );
        return 1;
    };
    
    sub put_tagline {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=tagline //;
        $symbols->{tagline}         = $in_line;
        return 1;
    };
    
    sub put_quote {
        # ignore current line
        my $q_line      ;
        my @quote_lines ;
        my $quote_cite  ;
        
        while ( $q_line = <$in_fh> ) {
            chomp $q_line;
            last if ( $q_line =~ /^[\s]*$/ );   # blank line terminates quote
            if ( $q_line =~ s/^[\s]*--// ) {    # '--' indicates citation
                $quote_cite     = $q_line;      #   and we stripped the '--'
                next;    
            };
            $q_line =~ s/^$pod_indent//;
            push @quote_lines, $q_line;         # otherwise it's part of quote
        };
        
        $symbols->{quote}         = join q{ }, @quote_lines;
        $symbols->{quote_cite}    = $quote_cite;
        
        return 1;
    };
    
    sub put_pod{
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/^#//;   # strip the octothorpe; now it's a POD directive
        
        # Be sure the previous line is correctly blank
        if ( @outlines ) {      # don't get burnt by undefined $outlines[-1]
            if ( $outlines[-1] ne q{} ) {
                push @outlines, q{};
            };
        };
        
        # Add the line, with a blank line following
        push @outlines, $in_line;
        push @outlines, q{};
        
        if ( $in_line =~ s/=head[\d]?// ) {    # include in mini-TOC
            push @{ $symbols->{cheats} }, $in_line;
        };
        
        return 1;
    };
    
    sub put_blank {
        push @outlines, q{};
        return 1;
    };
    
    sub put_code {
        my    $in_line      = shift;
        chomp $in_line;
        push @outlines, $pod_indent . $in_line;
        return 1;
    };
    
} ## metro make_pod




__END__
