#!/run/bin/perl
#       cheat-build.pl
#       =  Copyright 2010 Xiong Changnian <xiong@cpan.org>   =
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
use Storable qw( dclone );

#~ use Devel::Comments '###';
#~ use Devel::Comments '####';
#~ use Devel::Comments '#####';

use Template;

## use

# File name, path stuff
my $base_dirabs         = getcwd();
my $tmpl_dirrel         = q{file};      # where to find TT templates

my $raw_ext             = q{.agi};      # my source file
                                        #   (unrelated to Asterisk::AGI)
my $tmpl_ext            = q{.tt2};      # TT template file
my $pod_ext             = q{.pod};      # POD-only  module file
my $perl_ext            = q{.perl};     # Perl-only editor file

# Constants
my $pod_indent          = q{ } x 4;

## pseudo-globals

# Always do work in one place
chdir $base_dirabs
    or die "$0: Failed to chdir $base_dirabs.", $!;

# Get command-line arguments
my @files_todo          = @ARGV;

# For each file on command line...
for my $infile (@files_todo) {
    $infile =~ /$raw_ext$/
        or die  "$0: Attepted to parse $infile. ",
                "Specify only $raw_ext files. ", $!;
    init_parse();
    my $symbols     = parse($infile);
    ##### $symbols
    my $p_symbols   = dclone($symbols);
    make_pod ($infile, $p_symbols);
    my $q_symbols   = dclone($symbols);
    make_perl($infile, $q_symbols);
};

exit(0);
## end main code


{ # metro parse 

    # Shared variables...
    my $in_fh           ;   # filehandle of input file
    my $outlines        ;   # HoA where primary key is $head_key
    my $head_key        ;   # key to verbatim lines accumulator
    my $symbols         ;   # parsed output
    
    sub init_parse {
        $outlines        = {};       # HoA where primary key is $head_key
        $head_key        = q{intro}; # head of verbatim lines accumulator
        $symbols         = {
            package             => q{Cheat::},      # default ignored
            package_path        => q{Cheat/},       # default ignored
            tagline             => q{},
            quote               => q{},
            quote_cite          => q{},
            cheats              => [],
            outlines            => {},
        };
    }; ## init_parse

    sub parse {
        my $infile          = $_[0];
        
        # Process $infile line-by-line
        open $in_fh, '<', $infile
            or die "$0: Failed to open $infile to read.", $!;
        INLINE:
        while ( my $in_line = <$in_fh> ) {
            ### $head_key
            ### $in_line
            given ($in_line) {
                when (/^__END__$/)      { last INLINE }
                when (/^#-#/)           {}  # skip
                when (/^#=author /)     { put_author ($_) }
                when (/^#=license /)    { put_license($_) }
                when (/^#=package /)    { put_package($_) }
                when (/^#=tagline /)    { put_tagline($_) }
                when (/^#=quote/)       { put_quote  ($_) }
                when (/^#=head[\d]?/)   { put_head   ($_) }
                when (/^[\s]*$/)        { put_blank  ($_) }
                default                 { put_code   ($_) }
            };
        };
        close $in_fh
            or die "$0: Failed to close $infile.", $!;
        
        $symbols->{outlines}    = $outlines;
        
        return $symbols;
        
    }; ## parse
        
    # Helper subs...
    
    sub put_author {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=author[\s]*//;
        $symbols->{author}         = $in_line;
        return 1;
    };
    
    sub put_license {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=license[\s]*//;
        $symbols->{license}         = $in_line;
        return 1;
    };
    
    sub put_package {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=package[\s]*//;
        $symbols->{package}         = $in_line;
        $in_line =~ s|::|/|;
        $symbols->{package_path}    = $in_line;
        return 1;
    };
    
    sub put_tagline {
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/#=tagline[\s]*//;
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
        
        $symbols->{quote_lines}   = \@quote_lines;
        $symbols->{quote}         = join q{ }, @quote_lines;
        $symbols->{quote_cite}    = $quote_cite;
        
        return 1;
    };
    
    sub put_head{
        my    $in_line      = shift;
        chomp $in_line;
        $in_line =~ s/^#=head2//;   
        push @{ $symbols->{cheats} }, $in_line;     # include in mini-TOC
        $head_key           = $in_line;
        
        
        return 1;
    };
    
    sub put_blank {
        push @{ $outlines->{$head_key} }, q{};
        return 1;
    };
    
    sub put_code {
        my    $in_line      = shift;
        chomp $in_line;
        push @{ $outlines->{$head_key} }, $in_line;
        return 1;
    };
    
} ## metro parse


sub make_pod {
    my $infile      = shift;
    my $symbols     = shift;
    my @outlines    ;
    
    my $outfile     = $infile;
    $outfile =~ s/$raw_ext/$pod_ext/;
    my $tmpl_file   = q{pod} . $tmpl_ext;
    my $tmpl_fn_rel         = catfile ( $tmpl_dirrel, $tmpl_file );

    my $tt              = Template->new;
    #### $infile
    #### $outfile
    
    # Do a little preparation for POD output
    my @cheats      = @{ $symbols->{cheats} };
    for my $cheat (@cheats) {
        #### $cheat
        # Be sure the previous line is correctly blank
        if ( @outlines ) {      # don't get burnt by undefined $outlines[-1]
            if ( $outlines[-1] ne q{} ) {
                # First, try to salvage an improperly blank line
                if ( $outlines[-1] =~ /[\s]*/ ) {
                     $outlines[-1] = q{};
                }
                else {
                    push @outlines, q{};
                };
            };
        };
        
        # Push the =head2 cheat itself, with a blank line following
        push @outlines, q{=head2 } . $cheat;
        push @outlines, q{};
        
        # Push the verbatim content keyed to that cheat
        my @verbatim_lines      = @{ $symbols->{outlines}{$cheat} };
        for (@verbatim_lines) { s/^/$pod_indent/ };
        push @outlines, @verbatim_lines;
        
    }; ## for @cheats
    
    # Last-minute cleanup of trailing whitespace
    for (@outlines) { s/[\s]*$// };
    
    # And store it in a new key
    $symbols->{p_outlines}     = \@outlines;
    #### $symbols
    
    # Pass the stuff to Template, which opens, reads, and writes
    $tt->process( $tmpl_fn_rel, $symbols, $outfile )
        or do {
            warn $tt->error;
            die "$0: Failed to execute template process method.", $!;
        };
    
}; ## end make_pod

sub make_perl {
    my $infile      = shift;
    my $symbols     = shift;
    my @outlines    ;
    
    my $outfile     = $infile;
    $outfile =~ s/$raw_ext/$perl_ext/;
    my $tmpl_file   = q{perl} . $tmpl_ext;
    my $tmpl_fn_rel         = catfile ( $tmpl_dirrel, $tmpl_file );

    my $tt              = Template->new;
    #### $infile
    #### $outfile
    
    # Just aggregate all the cheats
    my @cheats      = @{ $symbols->{cheats} };
    for my $cheat (@cheats) {
        #### $cheat
        
        # Push the verbatim content keyed to that cheat
        my @verbatim_lines      = @{ $symbols->{outlines}{$cheat} };
        push @outlines, @verbatim_lines;
        
    }; ## for @cheats
    
    # And store it in a new key
    $symbols->{q_outlines}     = \@outlines;
    #### $symbols
    
    # Pass the stuff to Template, which opens, reads, and writes
    $tt->process( $tmpl_fn_rel, $symbols, $outfile )
        or do {
            warn $tt->error;
            die "$0: Failed to execute template process method.", $!;
        };
    
};

__END__
