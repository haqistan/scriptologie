#!/usr/bin/perl
##
# diffscend.pl - descend into directory trees to produce patches and files
#
# See the POD at EOF for documentation and license.
##
use strict;
use warnings;
use Pod::Usage;
use vars qw($P $COPY_YEARS $YEAR_WRITTEN $VERBOSE $DEFAULTS $VERSION $ALIASES);

##
## attila's ye olde scriptie boilerplate
##

BEGIN {
    ($P) = reverse(split('/',$0));
    my $yyyy = 1900+(localtime(time))[5];
    $YEAR_WRITTEN = 2015;
    $COPY_YEARS = sprintf(
        ($yyyy == $YEAR_WRITTEN) ? q{%d} : q{%d-%d}, $YEAR_WRITTEN, $yyyy
    );
    $VERBOSE = 0;
    $DEFAULTS = {
        'ts-fmt' => '%c',
        'diff-cmd' => 'diff -Nup',
        'exclude' => ['.', '..', '.git'],
    };
    $ALIASES = {
        'v' => 'verbose',
        'h' => 'help',
        'O' => 'overwrite',
        'X' => 'exclude',
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
    pod2usage(-verbose => 2)            if $VERBOSE && !defined($msg);
    if (defined($msg)) {
        print STDERR "$P: ERROR: $msg\n"     if defined $msg;
    } else {
        print STDERR "$P: recursively generate openbsd ports patch sets\n";
    }
    print STDERR "usage: $P [-options] [args]\n";
    print STDERR "       Options:\n";
    print STDERR "          -v --verbose        increment verbosity level\n";
    print STDERR "          -h --help           print this brief message\n";
    print STDERR "          -n --dry-run        just say what we would do\n";
    print STDERR "          -O --overwrite      overwrite existing patches\n";
    print STDERR "          --exclude=name      add name to list of excluded pathnames (def: ., .., .git)\n";
    print STDERR "          --ts-fmt=fmt        strftime(3) format for timestamps (def: %c)\n";
    print STDERR "          --diff-cmd=cmd      diff(1) command to use (def: diff -Nup)\n";
    print STDERR "          --patches=dir       where to drop patch files\n";
    print STDERR "          --files=dir         where to drop new/binary files\n";
    print STDERR "       To see the full documentation, try:\n\n";
    print STDERR "           \$ $P --help --verbose\n";
    exit(defined($msg)? 1:0);
}

# parse_argv - simplistic and effective CLA parser
#
# We turn @ARGV into a hashref, with the options given as the keys.
#
# If called with no args we start with an empty hashref; otherwise our
# argument should be a hashref and it becomes the starting point for
# our final product, allowing the caller to provide defaults and
# initialize values where necessary.
#
# Double-dash options can have values, e.g. --option=val; if none is
# given then the option is treated as an auto-increment/bool option.
# Single-dash options are single-letter as per ancient Unix custom and
# are split if they contain multiple characters, e.g. -vcd is
# shorthand for -v -c -d and will result in 'v', 'c', and 'd' keys in
# the result hashref.
#
# The special name '_' (underscore) collects any non-option arguments
# into an arrayref in the order that they appear regardless of
# options.  You cannot have an option named underscore.
#
# If an optoin can take multiple values (because it can be specified
# multiple times) then initialize it to an arrayref, e.g.
#
#    my $args = parse_argv({ 'thing' => [] });
#
# If the user invokes us thus:
#
#    $ command --thing=a --thing=b
#
# we return a hashref with 'thing' => ['a','b']
#
# If instead a hashref is given as the value then the option's value
# will be parsed as key:val, so
#
#    $args = parse_argv({ 'property' => {} });
#
# invoked with
#
#    $ command --property=color:black --property=size:small
#
# will result in a hashref with
#
#    'property' => { 'color' => 'black', 'size' => 'small' }
#
# Otherwise options have scalar values.  If no value is given on the
# command line the value is assumed to be an int and is incremented.
#
# After parsing @ARGV we apply any aliases in the global $ALIASES
# hashref, which defines aliases in the option namespace.  By default
# we have: 'v' => 'verbose', 'h' => 'help'.  After aliases, the
# $DEFAULTS hashref is applied, which maps option names to default
# values.  Finally we check for the special option name 'verbose' and
# set the global $VERBOSE to its value.  If the special option
# 'debug-options' is present we dump out the results of our work at
# the very end.
#
sub parse_argv {
    my $args;
    if (@_ && (ref($_[0]) eq 'HASH')) {
        $args = shift(@_);
    } else {
        $args = {};
    }
    $args->{'_'} ||= [];
    my @argv = @_;
    foreach my $arg (@argv) {
        $arg =~ s/^\s+//;
        $arg =~ s/\s+$//;
        next unless length $arg;
        if ($arg =~ /^(--[^=]+?)[=](.*)$/) {
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
        } elsif ($arg =~ /^(--.*)$/) {
            my $k = qchomp($1);
            $k =~ s/^-+//;
            if ($k ne '_') {
                ++$args->{$k};
            } else {
                usage(qq{Cannot have an option named underscore});
            }
        } elsif ($arg =~ /^-([^\-]+)$/) {
            ++$args->{$_} foreach (grep { $_ ne '_' } split('',qchomp($1)));
        } else {
            $args->{'_'} = [] unless defined $args->{'_'};
            push(@{$args->{'_'}}, $arg);
        }
    }
    # Now apply option aliases
    foreach (keys(%$ALIASES)) {
        $args->{$ALIASES->{$_}} = $args->{$_}
            if (defined($args->{$_}) && !defined($args->{$ALIASES->{$_}}))
    }
    # Next apply defaults, so aliases can take effect
    foreach (keys(%$DEFAULTS)) {
        $args->{$_} = $DEFAULTS->{$_} unless defined($args->{$_});
    }
    # Finally some skulduggery
    $VERBOSE = $args->{'verbose'};
    # All done
    if (defined($args->{'debug-options'})) {
        use Data::Dumper;
        local $Data::Dumper::Terse = 1;
        local $Data::Dumper::Indent = 0;
        warn(sprintf("$P: DEBUG-OPTIONS: '%s' => %s\n",$_,Dumper($args->{$_})))
             foreach (sort(keys(%$args)));
    }
    return $args;
}

##
## Application Logic
##

# dir - return a list of the contents of $dir
#
sub dir {
    my($state,$prefix,$dir) = @_;
    my %xclude = map { $_ => 1 } @{$state->{'exclude'}};
    my $dirpath = "$prefix/$dir";
    $dirpath =~ s/\/+$//;
    my $mydir = length($dir) ? "$dir/" : "";
    opendir(my $dh, "$dirpath") or die("$P: cannot opendir '$dirpath': $!\n");
    my @contents = (
        map { "$mydir$_" }
        grep { !defined($xclude{$_}) }
        readdir($dh)
    );
    closedir($dh);
    return @contents;
}

# ts - return a formatted timestamp given an optional epoch int
#
sub ts {
    my($state,$t) = @_;
    my $fmt = $state->{'ts-fmt'};
    $t = time() unless defined($t);     # zero is okay
    return POSIX::strftime($fmt,localtime($t));
}

# diffcmd - return the diff command to compare $dir1/$fn to $dir2/$fn
#
sub diffcmd {
    my($state,$dir1,$dir2,$fn) = @_;
    my $diff = $state->{'diff-cmd'};
    return sprintf("$diff $dir1/$fn $dir2/$fn");
}

# reset_patch - reset state for current patch
#
sub reset_patch {
    my($state) = @_;
    $state->{' base name'} = '';        # base name of patch
    $state->{' content'} = '';          # content of patch
    $state->{' saw names'} = 0;         # which of ---, +++ have we seen?
    $state->{' a dir'} = '';
    $state->{' b dir'} = '';
}

# remove1 - remove first component of path
#
sub remove1 {
    my($path) = @_;
    my @parts = split('/',$path);
    shift(@parts) if @parts > 1;
    return join('/',@parts);
}

# copy_file - copy $src to $dst w/optional $mode + mkdir -p
#
sub copy_file {
    my($state,$src,$dst,$mode) = @_;
    my @parts = split(/\//,$dst);
    pop(@parts);
    my $dir = join('/',@parts);
    my $verbo = undef;
    unless ((-d $dir) && !$state->{'dry-run'}) {
        system("mkdir -p $dir") == 0
            or die("$P: mkdir -p $dir: $?\n");
    }
    if (!(-f $dst) || $state->{'overwrite'}) {
        if ($state->{'dry-run'}) {
            $verbo = '[DRY-RUN] would copy';
        } else {
            system("cp $src $dst") == 0
                or die("$P: cp $src $dst: $?\n");
            $verbo = 'copied';
        }
    }
    if (!$state->{'dry-run'}) {
        chmod($mode,$dst) if defined($mode);
    }
    ++$state->{' ncopy'};
    return $verbo;
}

# new_file - drop a new file into the right place
#
sub new_file {
    my($state,$dir,$fn) = @_;
    if (!(-d $state->{'files'})) {
        warn("$P: WTF: no place to drop new file $dir/$fn\n");
    } else {
        my $path = "$dir/$fn";
        my $dest = join('/',$state->{'files'},remove1($path));
        my $verbo = copy_file($state,$path,$dest);
        warn("$P: $verbo to $dest - new file\n") if $VERBOSE;
    }
}

# patch_file_name - return the name to be used for the patch file
#
sub patch_file_name {
    my($state,$base) = @_;
    my $name = 'patch-' . $base;
    $name =~ s/\/+/_/g;
    $name =~ s/\./_/g;
    return $name;
}

# accum_patch - accumulate content into a patch
#
sub accum_patch {
    my($state,$line) = @_;
    chomp($line);
    if (!$state->{' saw names'}) {
        # This is how we seem to do it in the ports tree:
        #    --- Foo.orig   Sat Feb 28 14:13:39 2015
        #    +++ Foo        Sat Feb 28 14:13:39 2015
        if ($line =~ /^---\s(\S+)\s+(.*)$/) {
            my($name,$date) = (remove1($1),$2);
            $name .= '.orig';
            $line = "--- $name\t$date";
            $state->{' saw names'} = 1;
        }
    } elsif ($state->{' saw names'} == 1) {
        if ($line =~ /^\+\+\+\s(\S+)\s+(.*)$/) {
            my($name,$date) = (remove1($1),$2);
            $line = "+++ $name\t$date";
        } else {
            warn("$P: was expecting +++ after ---: '$line'\n");
        }
        $state->{' saw names'} = 2;
    } # else we're in the middle of a patch
    $state->{' content'} .= "$line\n";
    return undef;
}

# start_patch - start accumulating a patch
#
sub start_patch {
    my($state,$dir1,$dir2,$fn,$start) = @_;
    chomp($start);
    my $ok = 0;
    if ($start=~/^Binary\sfiles\s(\S+)\/(\S+)\sand\s(\S+)\/(\S+)\sdiffer/) {
        my($dir,$nm,$dir2,$n2) = ($1,$2,$3,$4);
        die(qq{$P: binary file names should be the same: $start\n})
            unless ($nm eq $n2);
        new_file($state,$dir,$nm);
    } else {
        $state->{' base name'} = $fn;
        my $cmd = diffcmd($state,$dir1,$dir2,$fn);
        $state->{' content'} = "$cmd\n";
        $state->{' a dir'} = $dir1;
        $state->{' b dir'} = $dir2;
        accum_patch($state,"$start\n");
        $ok = 1;
    }
    return $ok;
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

# patchify - turn diff output into a patch or new file
#
sub patchify {
    my($state,$dir1,$dir2,$fn) = @_;
    reset_patch($state);
    if (!(-f "$dir1/$fn") && (-f "$dir2/$fn")) {
        if (!(-d $state->{'files'})) {
            warn("$P: SKIPPED: $fn in $dir2 but not $dir2, no --files given\n");
        } else {
            new_file($state,$dir2,$fn);
        }
    }
    my $cmd = diffcmd($state,$dir1,$dir2,$fn);
    open(DIFF,"$cmd|") || die("$P: failed: $cmd: $?\n");
    my $line = <DIFF>;
    if (defined($line) && start_patch($state,$dir1,$dir2,$fn,$line)) {
        ++$state->{' line count'};
        while (defined($line = <DIFF>)) {
            accum_patch($state,$line);
        }
        finish_patch($state);
    } # else either no diff or start_patch did something clever
    close(DIFF);
}

# diffscend - descend into two directory trees and generate patches from diffs
#
sub diffscend {
    my($state,$dir1,$dir2) = @_;
    $state->{' ncopy'} = 0;
    $state->{' npatches'} = 0;
    $state->{' line count'} = 0;
    # Forward: diff from dir1 -> dir2
    warn("$P: Phase 1: $dir1 => $dir2 ...\n");
    my @queue = dir($state,$dir1,"");
    while (defined(my $fn = shift(@queue))) {
        if (-d "$dir1/$fn") {
            push(@queue,dir($state,$dir1,$fn));
        } else {
            patchify($state,$dir1,$dir2,$fn);
        }
    }
    # Backward: diff from dir2 -> dir1 to pick up new files
    warn("$P: Phase 2: $dir2 => $dir1 ...\n");
    @queue = dir($state,$dir2,"");
    while (defined(my $fn = shift(@queue))) {
        if (-d "$dir2/$fn") {
            push(@queue,dir($state,$dir2,$fn));
        } elsif (!(-f "$dir1/$fn")) {
            new_file($state,$dir2,$fn);
        } # else the first pass through should've caught it
    }
}

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    my @dirs = @{$args->{'_'}};
    usage("need two directories as arguments") unless @dirs == 2;
    diffscend($args,@dirs);
    if ($VERBOSE) {
        my($l,$n,$c) = map { $args->{" $_"} } ('line count','npatches','ncopy');
        warn("$P: $n patches found in $l lines of input, $c new files\n");
    }
    exit(0);
}

__END__

=pod

diffscend - recursive diff + openbsd patchfile generation

=head1 SYNOPSIS

  # turn the diffs between tor-browser and esr31 into patches + files
  $ diffscend -v --patches=my-port/patches \
                 --files=my-port/files/post-extract \
         mozilla-esr31 tor-browser

=head1 DESCRIPTION

This utility aids in turning the differences between two source trees
into a C<patches> and C<files> subdirectories suitable for use in an
OpenBSD port.

=head1 OPTIONS

We accept the following optionology:

=over 4

=item --verbose (or -v)

Increase the verbosity level of the script.

=item --help (or -h)

Print a short usage message.  If you specify -verbose, you get this
man page.

=item --dry-run (or -n)

Do not do anything, just say what we would've done.

=item --overwrite (or -O)

Overwrite existing patch files if they exist.  The default is to
complain about them but not touch them.

=item --patches=dir

Set the output directory for patch files; defaults to C<./patches>.
We complain if it does not exist uniless C<-dry-run> is specified.

=item --files=dir

If specified then binary files will not get patches, their contents
will be dropped into C<dir> with whatever subdirectory structure they
have preserved.

=back

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   23 Feb 15    attila   raised from the ashes of diffsplit.pl

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
