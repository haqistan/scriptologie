#!/usr/bin/perl
##
# overcopy.pl - conditionally copy/overwrite directorty trees
##
# Contact: attila@stalphonsos.com | 0x4FFCBB9C
# Encrypted/signed mail preferred.  ASCII armor uber alles.
##
# (C) 2002-2011 by attila <attila@stalphonsos.com>
# All Rights Reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
# See the POD at EOF for docs, or invoke with -help -verbose
##
use strict;
use warnings;
use Pod::Usage;
use Switch 'Perl6';
use Digest::MD5;
use vars qw($P $L $W $COPY_YEARS $VERBOSE $DEFAULTS $VERSION);

BEGIN {
    ($P) = reverse(split('/', $0)); # XXX File::Spec would be better
    $L = $ENV{'_SCRIPT_LEVEL'} ? $ENV{'_SCRIPT_LEVEL'} : 0;
    $W = ' ' x $L;
    my $yyyy = 1900+(localtime(time))[5];
    $COPY_YEARS = sprintf(($yyyy == 2007) ? q{%d} : q{%d-%d}, 2007, $yyyy);
    $VERBOSE = 0;
    $DEFAULTS = {
    };
    $VERSION = '0.1.0';
}

## qchomp - trim leading and trailing whitespace and deal with quoted strings
##
sub qchomp {
    my $str = shift(@_);
    while ($str =~ /^\s*([\"\'])(.*)\1\s*$/) {
        $str = $2;
    }
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    return $str;
}

## usage - dump a usage message and die
##
sub usage {
    my($msg) = @_;
    pod2usage(-verbose => 2)            if $VERBOSE && !defined($msg);
    if (defined($msg)) {
        print STDERR "$::P: $msg\n"     if defined $msg;
    } else {
        print STDERR "$::P: copy over contents\n";
    }
    print STDERR "usage: $::P [-options] [args]\n";
    print STDERR "       Standard options:\n";
    print STDERR "          -v|verbose      increment verbosity level\n";
    print STDERR "          -V|verbosity=n  set verbosity level to n\n\n";
    print STDERR "          -help           print this brief message\n";
    print STDERR "       To see the full documentation, try:\n\n";
    print STDERR "           \$ $::P -help -verbose\n";
    exit(defined($msg)? 1:0);
}

## parse_argv - simplistic and effective CLA parser
##
sub parse_argv {
    my $args;
    if (@_ && (ref($_[0]) eq 'HASH')) {
        $args = shift(@_);
    } else {
        $args = {};
    }
    my @argv = @_;
    foreach my $arg (@argv) {
        $arg =~ s/^\s+//;
        $arg =~ s/\s+$//;
        next unless length $arg;
        if ($arg =~ /^(-{1,2}[^=]+?)[=](.*)$/) {
            my($k,$v) = ($1,qchomp($2));
            $k =~ s/^-+//;
            if ($k ne '_') {
                if (!exists($args->{$k}) ||
                    (ref($args->{$k}) !~ /^(ARRAY|HASH)$/)) {
                    $args->{$k} = $v;
                } elsif (ref($args->{$k}) eq 'HASH') {
                    my($kk,$vv) = split(/:/,$v,2);
                    $args->{$k}->{$kk} = $vv;
                } else {
                    push(@{$args->{$k}}, $v);
                }
            } else {
                $args->{$k} = [] unless defined $args->{$k};
                push(@{$args->{$k}}, $v);
            }
        } elsif ($arg =~ /^(-{1,2}.*)$/) {
            my $k = qchomp($1);
            $k =~ s/^-+//;
            if ($k ne '_') {
                ++$args->{$k};
            } else {
                usage(qq{Cannot have an option named underscore});
            }
        } else {
            $args->{'_'} = [] unless defined $args->{'_'};
            push(@{$args->{'_'}}, $arg);
        }
    }
    $args->{'verbose'} = $args->{'v'}
        if (defined($args->{'v'}) && !defined($args->{'verbose'}));
    $args->{'dry-run'} = $args->{'n'}
        if (defined($args->{'n'}) && !defined($args->{'dry-run'}));
    $VERBOSE = $args->{'verbose'} || 0;
    return $args;
}

## get_arg - get a named command-line argument and return a default if missing
##
sub get_arg {
    my($args,$name,$def) = @_;
    my $arg = $def;
    $arg = $args->{$name} if exists($args->{$name});
    return $arg;
}

## get_pos - get positional argument
##
sub get_pos {
    my($args,$idx,$def) = @_;
    my $n = scalar(@{$args->{'_'}});
    return $def unless (defined($idx) && ($idx >= 0) && ($idx < $n));
    return $args->{'_'}->[$idx];
}

## stash - stash away name=val somewhere
##
sub stash {
    my($args,$name,$val) = @_;
    my $stash = " $name";
    $args->{$stash} = $val;
}

## stashed - return the stashed value of $name or $def if there is none
##
sub stashed {
    my($args,$name,$def) = @_;
    my $stash = " $name";
    my $val = $def;
    $val = $args->{$stash} if exists($args->{$stash});
    return $val;
}

## get_opt - boolify the result of get_arg
##
sub get_opt {
    my $val = get_arg(@_);
    return $val ? 1 : 0;
    
}

## run - run a command, maybe
##
sub run {
    my($args,$cmd,$force) = @_;
    if ($cmd) {
        if (!$force && get_opt($args,'dry-run',0)) {
            warn("# $W$P: dry-run: $cmd\n")
                if $VERBOSE;
        } else {
            warn("# $W$P: run: $cmd\n")
                if $VERBOSE;
            local $ENV{'_SCRIPT_LEVEL'} = 1+$L;
            system($cmd) == 0
                or die("$cmd: $!\n");
        }
    }
}

## find_all_files - find all files in a directory tree w/optional regexp ext
##
sub find_all_files {
    my($args,$subdir,$regexp) = @_;
    my @files = ();
    if (opendir(SUBDIR,$subdir)) {
        my @list = (
            grep {
                ($_ !~ /^[\.\#]/) && ($_ !~ /~$/) &&
                ((-d "$subdir/$_") || !$regexp || ($_ =~ /\.$regexp$/))
            } readdir(SUBDIR)
        );
        closedir(SUBDIR);
        push(@files, map { "$subdir/$_" } grep { -f "$subdir/$_" } @list);
        push(@files, find_all_files($args,"$subdir/$_",$regexp))
            foreach (grep { -d "$subdir/$_" } @list);
    }
    return @files if wantarray;
    return join("\n",@files)."\n";
}

## sizes_differ - return true if the sizes of two files differ
##
sub sizes_differ {
    my($a,$b) = @_;
    return -1 if  defined($a) && !defined($b);
    return  1 if  defined($b) && !defined($a);
    return -2 if !defined($a) && !defined($b);
    return ($a->[7] - $b->[7]);
}

## checksums_differ - return true if the MD5's of two files differ
##
sub checksums_differ {
    my($f1,$f2) = @_;
    open(F1,$f1) or die("$f1: $!\n");
    binmode(F1);
    my $d1 = Digest::MD5->new->addfile(*F1)->hexdigest;
    close(F1);
    open(F2,$f2) or die("$f2: $!\n");
    binmode(F2);
    my $d2 = Digest::MD5->new->addfile(*F2)->hexdigest;
    close(F2);
    return ($d1 ne $d2);
}

## are_different - are two files different enough to copy src to dst?
##
sub are_different {
    my($src,$dst) = @_;
    my $s1 = [stat($src)];
    my $s2 = [stat($dst)];
    my $diff = sizes_differ($s1,$s2) || checksums_differ($src,$dst);
    return $diff;
}

## action - turn a filename into a [action,partial-path] vector
##
sub action {
    my($args,$file) = @_;
    my $src_root = get_pos($args,0)
        or usage("missing positional argument: src dir");
    my $dst_root = get_pos($args,1)
        or usage("missing positional argument: dst dir");
    my $ppath = $file;
    $ppath =~ s/^${src_root}//;
    $ppath =~ s/^\/+//;
    $dst_root =~ s/\/+$//;
    my $dfile = join('/',$dst_root,$ppath);
    my $diff = [];
    if (!(-f "$dst_root/$ppath")) {
        push(@$diff,'+',$ppath);
    } elsif (are_different($file,$dfile)) {
        push(@$diff,'+',$ppath);
    } else {
        push(@$diff,undef,$ppath);
    }
    return $diff;
}

## list_diffs - form a list of actions to perform given our arguments
##
sub list_diffs {
    my($args) = @_;
    my $src_root = get_pos($args,0,'.');
    my $excl = get_arg($args,'exclude','');
    my $ext = get_arg($args,'ext');
    my @list = find_all_files($args,$src_root,$ext);
    warn("# $W$P: found ".scalar(@list)." total files\n")
        if $VERBOSE;
    if ($VERBOSE > 1) {
        warn("# $W$P:   $_\n") foreach (@list);
    }
    if ($excl) {
        warn("# $W$P: processing exclusion: $excl\n") if $VERBOSE;
        @list =  grep { $_ !~ /$excl/ } @list;
    }
    warn("# $W$P: left with ".scalar(@list)." after exclusions\n")
        if $VERBOSE && $excl;
    @list = (
        grep { defined($_->[0]) }
        map { action($args,$_) }
        @list
    );
    warn("# $W$P: finally have ".scalar(@list)." after checking diffs\n")
        if $VERBOSE;
    return @list;
}

## copy_file - copy a file from the src root to the dst root
##
sub copy_file {
    my($args,$ppath) = @_;
    my $src_root = get_pos($args,0,'.');
    my $dst_root = get_pos($args,1,'/');
    $src_root =~ s/\/+$//;
    $dst_root =~ s/\/+$//;
    $ppath =~ s/^\/+//;
    my $sfile = join('/',$src_root,$ppath);
    my $dfile = join('/',$dst_root,$ppath);
    my @parts = split('/',$dfile);
    pop(@parts);
    my $dir = join('/',@parts);
    my $cmd;
    unless (-d $dir) {
        my $mkdir_p = get_arg($args,'mkdir','/bin/mkdir -p');
        $cmd = qq{$mkdir_p $dir};
        run($args,$cmd);
    }
    my $cp = get_arg($args,'cp','/bin/cp');
    $cmd = qq{$cp $sfile $dfile};
    run($args,$cmd);
    return $dfile;
}

## remove_file - remove a file from the dest root (not used right now)
##
sub remove_file {
    my($args,$ppath) = @_;
    die("can't happen");
}

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    my @list = list_diffs($args);
    foreach (@list) {
        my($what,$ppath) = @$_;
        given ($what) {
            when ('+') { copy_file($args,$ppath); }
            when ('-') { remove_file($args,$ppath); }
            default { die("$P: unknown code \"$what\" for $ppath\n"); }
        }
    }
    exit(1) if (get_opt($args,'dry-run',0) && @list);
    exit(0);
}

__END__

=pod

=head1 NAME

overcopy - copy over files when needed

=head1 SYNOPSIS

  # copy all files rooted in . that have the same names
  # as files under / but different contents/sizes
  $ overcopy . /

=head1 DESCRIPTION

This script is given a source tree and a destination root.  It first
finds all files in the source tree (minus any exclusions, specified by
a regular expression).  It then examines each file found.  After
stripping the source directory root from the front of the complete
path it appends this partial path to the destination root and checks
any file that is there.  If the source and destination files so formed
differ in size or in the checksum of their contents then the
destination is replaced with the source.  If the destination file does
not exist then the source file is copied there with any intervening
subdirectories that are missing created along the way.

=head1 OPTIONS

We accept the following optionology:

=over 4

=item -exclude=regexp

If specified then exclude any source files that match the regular
expression given.

=item -ext=regexp

If specified then only process files in the source tree whose
extension matches regexp.

=item -dry-run

=item -n

Do not actually copy anything only say what you would've done if
C<-verbose> is turned on.  We exit with a zero exit status if nothing
would've been done and non-zero otherwise.

=item -verbose (or -v)

=item -verbosity=int (or -V=int)

The first form increments the verbosity level every time it is seen.
The second form sets the verbosity level to the integer specified.
Higher verbosity levels mean more output.

=item -quiet

Be quiet about everything but errors.

=item -help

Print a short usage message.  If you specify -verbose, you get this
man page.

=item -version

Print our version

=back

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   26 Nov 11     attila  Started

=cut

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# perl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# End:
##
