#!/usr/bin/perl
##
# diffsplit.pl - split a file full of diffs into patch files
#
# See the POD at EOF for documentation and license.
##
use strict;
use warnings;
use Pod::Usage;
use POSIX;
use vars qw($P $YEAR_WRITTEN $COPY_YEARS $VERBOSE $DEFAULTS $VERSION);

##
## attila's ye olde scriptie boilerplate
##

BEGIN {
    ($P) = reverse(split('/',$0)); # XXX File::Spec would be better
    my $yyyy = 1900+(localtime(time))[5];
    $YEAR_WRITTEN = 2015;
    $COPY_YEARS = sprintf(
        ($yyyy == $YEAR_WRITTEN) ? q{%d} : q{%d-%d}, $YEAR_WRITTEN, $yyyy
    );
    $VERBOSE = 0;
    $DEFAULTS = {
    };
    $VERSION = '0.1.0';
}

# qchomp - trim leading and trailing whitespace and deal with quoted strings
#
sub qchomp {
    my $str = shift(@_);
    while ($str =~ /^\s*([\"\'])(.*)\1\s*$/) {
        $str = $2;
    }
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    return $str;
}

# usage - dump a usage message and die
#
sub usage {
    my($msg) = @_;
    pod2usage(-verbose => 2)            if $::VERBOSE && !defined($msg);
    if (defined($msg)) {
        print STDERR "$::P: $msg\n"     if defined $msg;
    } else {
        print STDERR "$::P: split a file of diffs into individual patchfiles\n";
    }
    print STDERR "usage: $::P [-options] [args]\n";
    print STDERR "       Standard options:\n";
    print STDERR "          -v|verbose      increment verbosity level\n";
    print STDERR "          -help           print this brief message\n";
    print STDERR "          -patches=dir    write patches to dir\n";
    print STDERR "          -overwrite      overwrite existing patch files\n";
    print STDERR "          -srcdir=dir     look in dir for source files\n";
    print STDERR "          -n|dry-run      just say what we would do\n\n";
    print STDERR "       To see the full documentation, try:\n\n";
    print STDERR "           \$ $::P -help -verbose\n";
    exit(defined($msg)? 1:0);
}

# parse_argv - simplistic and effective CLA parser
#
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
    $::VERBOSE = $args->{'verbose'};
    return $args;
}

##
## Application Logic
##

# reset_patch - reset state for current patch
#
sub reset_patch {
    my($state) = @_;
    $state->{' base name'} = '';        # base name of patch
    $state->{' content'} = '';          # content of patch
    $state->{' saw names'} = 0;         # which of ---, +++ have we seen?
}

# patch_file_name - return the name to be used for the patch file
#
sub patch_file_name {
    my($state,$base) = @_;
    my $name = 'patch-';
    if ($base =~ /^\./) {
        $name .= 'dot_';
        $base = substr($base,1);
    }
    $name .= $base;
    $name =~ s/\/+/_/g;
    $name =~ s/\./_/g;
    return $name;
}

# start_patch - start accumulating a patch
#
sub start_patch {
    my($state,$start) = @_;
    chomp($start);
    my $lno = $state->{' line count'};
    my($a,$b) = ($1,$2) if $start =~ /^diff\s+--git\s+a\/(\S+)\s+b\/(\S+)$/;
    my $ok = 0;
    if (!defined($a) || !defined($b)) {
        warn("$P: input line #$lno: malformed header (ignored): $start\n");
    } elsif ($a ne $b) {
        warn("$P: input line #$lno: a ($a) and b ($b) do not match\n");
    } else {
        $state->{' base name'} = $a;
        $state->{' content'} = '';
        $ok = 1;
    }
    return $ok;
}

# ts - return a formatted timestamp given an optional epoch int
#
sub ts {
    my($state,$t) = @_;
    my $fmt = $state->{'patch-ts-fmt'} || "%c";
    $t ||= time;
    return POSIX::strftime($fmt, localtime($t));
}

# file_ts - if 2nd arg names an existing file return its mtime
#
sub file_ts {
    my($state,$fn) = @_;
    my $path = $fn if -f $fn;
    if (!$path && defined($state->{'srcdir'})) {
        $path = join('/',$state->{'srcdir'},$fn);
    }
    return ts($state,time) unless -f $path;
    return ts($state,(stat($path))[8]);
}

# accum_patch - accumulate content into a patch
#
sub accum_patch {
    my($state,$line) = @_;
    my $date = ts($state);
    if (!$state->{' saw names'}) {
        if ($line =~ /^---\s(\S+)$/) {
            chomp(my $name = $1);
            if ($name =~ /^a\/(.*)$/) {
                $date = file_ts($state,$1);
                $name = $1 . '.orig';
            } elsif ($name eq '/dev/null') {
                $date = ts($state,0);
            }
            $line = "--- $name\t$date\n";
            $state->{' saw names'} = 1;
        }
    } elsif ($state->{' saw names'} == 1) {
        if ($line =~ /^\+\+\+\s(\S+)$/) {
            chomp(my $name = $1);
            if ($name =~ /^b\/(.*)$/) {
                $name = $1;
                $date = file_ts($state,$name);
            } elsif ($name eq '/dev/null') {
                $date = ts($state,0);
            }
            $line = "+++ $name\t$date\n";
        } else {
            my $lno = $state->{' line count'};
            warn("$P: input line #$lno: was expecting +++ after --- ...\n");
        }
        $state->{' saw names'} = 2;
    } # else nothing
    $state->{' content'} .= $line;
}

# finish_patch - spit out a complete patch file
#
sub finish_patch {
    my($state) = @_;
    my $nm = $state->{' base name'};
    my $patches_dir = $state->{'patches'} || 'patches';
    die("$P: directory '$patches_dir' does not exist or is not a directory\n")
        unless (-d $patches_dir);
    my $patch_name = patch_file_name($state,$nm);
    my $patch_path = join('/',$patches_dir,$patch_name);
    if (!$state->{'overwrite'} && (-f $patch_path)) {
        warn("$P: will not overwrite $patch_path - use --overwrite\n");
    } elsif ($state->{'dry-run'}) {
        my $nbytes = length($state->{' content'});
        my $nlines = split(/\n/,$state->{' content'});
        warn("$P: DRY-RUN: $patch_name: $nlines lines in $nbytes bytes\n");
        ++$state->{' npatches'};
    } else {
        open(PATCH,"> $patch_path")
            or die("$P: could not open $patch_path for writing\n");
        print PATCH '$OpenBSD$' . "\n\n";
        print PATCH $state->{' content'};
        close(PATCH);
        if ($VERBOSE) {
            my $nbytes = length($state->{' content'});
            my $nlines = split(/\n/,$state->{' content'});
            warn("$P: $patch_name: $nlines lines in $nbytes bytes\n");
        }
        ++$state->{' npatches'};
    }
    reset_patch($state);
}

# diffsplit - filter stdin into patch files
#
sub diffsplit {
    my($state) = @_;
    my $inside = 0;
    reset_patch($state);
    $state->{' line count'} = 0;
    $state->{' npatches'} = 0;
    while (defined(my $line = <STDIN>)) {
        ++$state->{' line count'};
        if ($line =~ /^diff\s+--git/) {
            finish_patch($state) if $inside;
            $inside = start_patch($state,$line);
        } elsif ($inside) {
            accum_patch($state,$line);
        }
    }
    finish_patch($state) if $inside;
}

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    diffsplit($args);
    if ($VERBOSE) {
        my($l,$n) = ($args->{' line count'},$args->{' npatches'});
        warn("$P: $n patches found in $l lines of input\n");
    }
    exit(0);
}

__END__

=pod

diffsplit - split file with multiple git diffs into patch files

=head1 SYNOPSIS

  # turn git diffs into OpenBSD ports-style patch files:
  $ git diff | diffsplit --patches=/path/to/port/patches

=head1 DESCRIPTION

This program accepts the output of C<git diff> on standard input and
turns it into multiple C<patch-*> files as per OpenBSD ports.

=head1 OPTIONS

We accept the following optionology:

=over 4

=item -verbose (or -v)

=item -verbosity=int (or -V=int)

The first form increments the verbosity level every time it is seen.
The second form sets the verbosity level to the integer specified.
Higher verbosity levels mean more output.

=item -overwrite

Overwrite existing patch files if they exist.  The default is to
complain about them but not touch them.

=item -patches=dir

Set the output directory for patch files; defaults to C<./patches>.
We complain if it does not exist uniless C<-dry-run> is specified.

=item -dry-run (or -n)

Do not write anything, just say what you would've done.

=item -srcdir=dir

If specified look in C<dir> for source files when trying to come up
with timestamps.  Kind of lame but better than nothing.  Defaults to
the current working directory.

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

  0.1.0   19 Feb 15     attila          written for tor-browser port

=head1 LICENSE

Copyright (C) 2015 by attila <attila@stalphonsos.com>

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

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
