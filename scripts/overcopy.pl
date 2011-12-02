#!/usr/bin/perl
##
# overcopy.pl - conditionally copy/overwrite directory trees
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
    $VERSION = '0.1.1';
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

## single_letter - allow a single-letter opt to be an alias for a word opt
##
sub single_letter {
    my($args,$word,$letter) = @_;
    $args->{$word} = $args->{$letter}
        if (defined($args->{$letter}) && !defined($args->{$word}));
}

## dump_args - for debugging
##
sub dump_args {
    my($args) = @_;
    my @keys = sort { $a cmp $b } grep { $_ !~ /^\s|^_$/ } keys(%$args);
    warn(
        "# $W$P: ".(scalar(@keys)-1).
        " named + ".scalar(@{$args->{'_'}}).
        " positional arguments:\n"
    );
    warn("# $W$P:   #$_: ".$args->{'_'}->[$_]."\n")
        foreach (0 .. (scalar(@{$args->{'_'}})-1));
    foreach my $k (@keys) {
        my $v = $args->{$k};
        given (ref($v)) {
            when ('ARRAY') { warn("# $W$P:   $k: [".join(", ",@$v)."]\n"); }
            when ('HASH') {
                warn(
                    "# $W$P:   $k: {".
                    join(
                        ", ",
                        map { sprintf("'%s' => \"%s\"",$_,$v->{$_}) }
                        sort { $a cmp $b } keys(%$v)
                    )."}\n"
                );
            }
            default { warn("# $W$P:   $k: \"$v\"\n"); }
        }
    }
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
        if ($arg =~ /^(-{1,2}[^=]+?)(=|\+=)(.*)$/) {
            my($k,$o,$v) = ($1,$2,qchomp($3));
            $k =~ s/^-+//;
            my $cur = exists($args->{$k}) ? $args->{$k} : undef;
            if ($k ne '_') {
                if (!exists($args->{$k})) {
                    if ($o eq '+=') {
                        $args->{$k} = [ $v ];
                    } else {
                        $args->{$k} = $v;
                    }
                } elsif (($o eq '+=') || ref($cur)) {
                    if (!ref($cur)) {
                        $args->{$k} = [ $cur, $v ];
                    } elsif (ref($cur) eq 'ARRAY') {
                        push(@$cur,$v);
                    } elsif (ref($cur) eq 'HASH') {
                        my($kk,$vv) = split(/:/,$v,2);
                        $cur->{$kk} = $vv;
                    } else {
                        usage("option $k$o$v: $k is a ".ref($cur));
                    }
                } else {
                    warn(
                        "# $W$P: option $k specified more than once ".
                        "(".$args->{$k}.": $v)\n"
                    );
                    $args->{$k} = $v;
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
    single_letter($args,'verbose','v');
    single_letter($args,'dry-run','n');
    single_letter($args,'forwards','F');
    single_letter($args,'backwards','B');
    single_letter($args,'both','b');
    single_letter($args,'confirm','C');
    single_letter($args,'prune','P');
    $VERBOSE = $args->{'verbose'} || 0;
    dump_args($args) if get_opt($args,'debug',0);
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

## get_opt - boolify the result of get_arg
##
sub get_opt {
    my $val = get_arg(@_);
    return $val ? 1 : 0;
}

## stash - stash away name=val somewhere
##
## We re-use the args hashref as a scratchpad with the convention that
## keys whose name starts with a space are "hidden" and used for
## scratch space.
##
sub stash {
    my($args,$name,$val) = @_;
    my $stash = " $name";
    $args->{$stash} = $val;
    return $val;
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
## We first do a cheap check (sizes) and then an expensive one (md5
## checksums) if the first one fails to detect a difference.  We
## *could* use modification times but this causes problems when clocks
## are out of sync.  Checksums are more expensive but sure to work.
##
sub are_different {
    my($src,$dst) = @_;
    return sizes_differ([stat($src)],[stat($dst)]) ||
        checksums_differ($src,$dst);
}

## root_and_other - given $idx return the corresponding dir and its opposite
##
## So if $idx == 0 then we return (src,dst) and if $idx == 1 we return
## (dst,src).  Used by tree_to_tree_path(), below.
##
sub root_and_other {
    my($args,$idx) = @_;
    my($iname,$oname) = '$idx' ? ('src','dst') : ('dst','src');
    my $root = get_pos($args,$idx)
        or usage("missing positional argument: $iname");
    my $other = get_pos($args,$idx? 0: 1)
        or usage("missing positional argument: $oname");
    return($root,$other);
}

## tree_to_tree_path - translate $file from its root folder into another
##
## If our source directory is /home/me/src/foo/htdocs and our
## destination directory is /var/www/vhosts/foo.com/htdocs then given
## $root_idx = 0 and $file = /home/me/src/foo/htdocs/images/poop.png
## we return /var/www/vhosts/foo.com/htdocs/images/poop.png
##
sub tree_to_tree_path {
    my($args,$file,$root_idx) = @_;
    my($root,$other) = root_and_other($args,$root_idx);
    my $ppath = $file;
    $ppath =~ s/^${root}//;
    $ppath =~ s/^\/+//;
    $other =~ s/\/+$//;
    my $dfile = join('/',$other,$ppath);
    return($ppath,$dfile);
}

##
## Forwards and Backwards
##
## Forwards: copying from the source tree to the destination tree.
## Backwards: removing from the destination what is not in the source
##
## "Actions" are just [code,partial-path] vectors where code is
## '+' for copy and '-' for delete.
##

## forwards - return a forward action for $file
##
sub forwards {
    my($args,$file) = @_;
    my($ppath,$dfile) = tree_to_tree_path($args,$file,0);
    my $diff = undef;
    if (!(-f "$dfile") || are_different($file,$dfile)) {
        $diff = [ '+', $ppath ];
    }
    return $diff;
}

## backwards - return a backward action for $file
##
sub backwards {
    my($args,$file) = @_;
    my($ppath,$sfile) = tree_to_tree_path($args,$file,1);
    my $diff;
#    warn("# $W$P: backwards($file): sfile=$sfile".((-f $sfile) ? " exists": " does not exist")."\n");
    if (!(-f $sfile)) {
        $diff = [ '-', $ppath ];
    }
    return $diff;
}

## list_diffs - form a list of actions to perform given our arguments
##
sub list_diffs {
    my($args) = @_;
    my $src_root = get_pos($args,0,'.');
    my $dst_root = get_pos($args,1,'/');
    my $excl = get_arg($args,'exclude',[]);
    my $ext = get_arg($args,'ext');
    ## Forward direction: find files in source tree that
    ## are not in destination tree or are different there
    ## and mark them for copying
    ## Backward direction: find files in destination tree that are not
    ## in source tree and mark them for deletion
    my @fwd = find_all_files($args,$src_root,$ext)
        if get_opt($args,'fowards',1) || get_opt($args,'both',0);
    warn("# $W$P: found ".scalar(@fwd)." total files forwards\n")
        if $VERBOSE;
    my @bwd = find_all_files($args,$dst_root,$ext)
        if get_opt($args,'backwards',0) || get_opt($args,'both',0);
    warn("# $W$P: found ".scalar(@bwd)." total files backwards\n")
        if $VERBOSE;
    if ($VERBOSE > 1) {
        warn("# $W$P:   $_\n")
            foreach (map { '> '.$_ } @fwd,map { '< '.$_ } @bwd);
    }
    if (@$excl) {
        foreach my $x (@$excl) {
            warn("# $W$P: processing exclusion: $x\n") if $VERBOSE;
            @fwd = grep { $_ !~ /$x/ } @fwd;
            @bwd = grep { $_ !~ /$x/ } @bwd;
        }
    }
    warn(
        "# $W$P: left with ".sprintf(q{%d+%d},scalar(@fwd),scalar(@bwd)).
        " after exclusions\n"
    ) if $VERBOSE && @$excl;
    @fwd = map { forwards($args,$_) } @fwd;
    @bwd = map { backwards($args,$_) } reverse(@bwd);
    my @list = grep { defined } (@fwd,@bwd);
    warn("# $W$P: finally ".scalar(@list)." actions after checking diffs\n")
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

## ask_about_removing - ask the user if we can remove $file
##
## We die with an error if STDIN is not a TTY or if we encounter EOF
##
sub ask_about_removing {
    my($args,$dfile) = @_;
    die("! $W$P: -confirm=ask specified but STDIN is not a TTY\n")
        unless (-t STDIN);
    my $answer = undef;
    do {
        print("# $W$P: CONFIRM deletion of $dfile [y/n/Y/N/?] : ");
        my $ans = <STDIN>;
        die("! $W$P: EOF encountered while confirming deletion of $dfile\n")
            unless defined($ans);
        $ans =~ s/\s//gs;
        given ($ans) {
            when /^y/ { $answer = 1; }
            when /^n/ { $answer = 0; }
            when /^Y/ { $answer = stash($args,'confirmed',1); }
            when /^N/ { $answer = stash($args,'confirmed',0); }
            default {
                warn("# $W$P: invalid input \"$ans\"\n") unless $ans eq '?';
                warn("# $W$P:  y = confirm this deletion\n");
                warn("# $W$P:  n = do not allow this deletion\n");
                warn("# $W$P:  Y = confirm ALL deletions from now on\n");
                warn("# $W$P:  N = disallow ALL deletions from now on\n");
                warn("# $W$P:  ? = display this message\n");
            }
        }
    } while (!defined($answer));
    return $answer;
}

## try_pruning - see if we can rmdir a directrory we just unlinked from
##
sub try_pruning {
    my($args,$dfile) = @_;
    my @tarts = split(/\//,$dfile);
    pop @tarts;                     # why can't they make them w/o gelatin?
    my $dir = join('/',@tarts);
    my @files = grep { (! -d $_) } find_all_files($args,$dir);
    unless (@files) {
        warn("# $W$P: removing empty directory: $dir\n")
            if $VERBOSE;
        run($args,"rm -rf $dir");
    }
}

## remove_file - remove a file from the dest root (not used right now)
##
sub remove_file {
    my($args,$ppath) = @_;
    my $conf = get_arg($args,'confirm');
    my($dst) = root_and_other($args,1);
    my $dfile = "${dst}/${ppath}";
    ## If the user just says -confirm then we'll have $conf = 1:
    $conf = 'ask' if (defined($conf) && ($conf eq '1'));
    ## If we have a stashed answer then carry on (N.B. zero != undef)
    my $is_confirmed = stashed($args,'confirmed');
    if (!defined($is_confirmed)) {
        if (!$conf) {                   # not specified - whinge
            warn("# $W$P: - $dfile\n")
                unless get_opt($args,'quiet',0);
        } elsif ($conf eq 'ask') {      # ask the user
            $is_confirmed = ask_about_removing($args,$dfile);
        } elsif ($conf eq 'delete')  {  # just blow it away
            stash($args,'confirmed',1);
            warn("# $W$P: confirmed all deletions\n")
                if $VERBOSE;
        } else {
            usage("unrecognized value for -confirm: $conf");
        }
    }
    if ($is_confirmed) {
        unlink($dfile) == 1 or
            warn("# $W$P: unlink($dfile): $!\n");
        warn("# $W$P: -- $dfile\n") if $VERBOSE;
        try_pruning($args,$dfile) if get_opt($args,'prune',0);
    }
}

##
## Main program
##

MAIN: {
    my $args = parse_argv({'_' => [],'exclude' => []}, @ARGV);
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

overcopy - copy over and/or remove files when needed

=head1 SYNOPSIS

  # copy all files rooted in . that have the same names
  # as files under /some/where but different contents/sizes
  $ overcopy . /some/where

=head1 DESCRIPTION

This script is given a source tree and a destination root.  It can
copy forwards, backwards, or both.

B<FORWARDS>: Find all files in the source tree (minus any exclusions,
specified by a regular expression).  Examine each file found.  After
stripping the source directory root from the front of the complete
path it appends this partial path to the destination root and check
any file that is there.  If the source and destination files so formed
differ in size or in the checksum of their contents then the
destination is replaced with the source.  If the destination file does
not exist then the source file is copied there with any intervening
subdirectories that are missing created along the way.

B<BACKWARDS>: Find all files in the destination tree (minus any
exclusions).  Do the same thing with stripping directories but in the
other direction: we remove the destination from the front and prepend
the source.  If after doing so the corresponding file in the source
tree odes not exist remove the file from the destination tree.

The default is to copy forwards but not backwards; command-line
options can force us to do either or both.  If backwards is enabled
then nothing will be removed unless the C<-confirm=delete> option is
also given; otherwise we will win but emit messages for each file we
would've deleted instead of actually doing it.  This prevents errors
between brain and keyboard from trashing your filesystem when you do
something like:

  # overcopy -b . /

(perhaps inadvertently, like from a buggy Makefile).

=head1 OPTIONS

We accept the following optionology:

=over 4

=item -forwards (alias: -F)

=item -backwards (alias: -B)

=item -both (alias: -b)

Enable or disable a direction; to disable forwards say C<-forwards=0>
as it is on by default.  C.f. the C<-confirm> option.

=item -exclude=regexp

If specified then exclude any source files that match the regular
expression given.  Multiple exclusion patterns can be specified by
specifyihg C<-exclude> more than once; they will all be tried, in the
order specified.

=item -ext=regexp

If specified then only process files in the source tree whose
extension matches regexp.

=item -confirm=conf (alias: -C)

Enable the deletion of files in the destination directory in some way;
if this option is not specified then the C<-backwards> option will
only emit warnings instead of remove files.  Conf may be one of:

=over 8

=item delete: delete files without asking

=item ask: ask whether or not to delete files

=back

If only C<-confirm> is specified as if it were a boolean option then
it is as if the user said C<-confirm=ask>.  If it is not possible to
ask the user a question (because stdin is not a TTY) then we fail with
an error when we try.

=item -prune (alias: -P)

Prune any empty directories in the destination directory recursively,
meaning that we remove empty directories and then march up the tree to
find any newly-emptied directories.  Only make sense if C<-backward>
is enabled.

=item -dry-run (alias: -n)

Do not actually copy anything only say what you would've done if
C<-verbose> is turned on.  We exit with a zero exit status if nothing
would've been done and non-zero otherwise.

=item -verbose (alias: -v)

=item -verbosity=int (alias: -V=int)

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

  0.1.1   01 Dec 11     attila  Added forwards, backwards, both
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
