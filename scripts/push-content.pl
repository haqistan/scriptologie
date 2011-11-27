#!/usr/bin/perl
##
# push-content.pl - install content from a git repo's master branch
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
use vars qw($P $W $L $COPY_YEARS $VERBOSE $DEFAULTS $VERSION);

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
        print STDERR "$::P: install from the master branch of a git repo\n";
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
    $VERBOSE = $args->{'verbose'};
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
            warn("# $W$P dry-run: $cmd\n")
                if $VERBOSE;
        } else {
            warn("# $W$P run: $cmd\n")
                if $VERBOSE;
            local $ENV{'_SCRIPT_LEVEL'} = 1+$L;
            system($cmd) == 0
                or die("$cmd: $!\n");
        }
    }
}

## checkout_master - check out the master branch from git into a tmp dir
##
sub checkout_master {
    my($args) = @_;
    my $repo_root = get_arg($args,'repo-root','/data/repo/git');
    my $repo_name = get_arg($args,'repo');
    die("$0: no repository name given in --repo\n") unless $repo_name;
    my $repo_path = $repo_root . '/' . $repo_name;
    $repo_path .= '.git' if (!(-d $repo_path) && (-d "${repo_path}.git"));
    die("$0: repo_path $repo_path is bad\n") unless (-d $repo_path);
    stash($args,'repo_path',$repo_path);
    my $tmp_root = get_arg($args,'tmpdir','/tmp');
    my $tmp_dir = $tmp_root . "/${repo_name}.$$.tmp";
    die("$0: tmp_dir $tmp_dir exists!?\n") if (-d $tmp_dir || -f $tmp_dir);
    mkdir($tmp_dir,0755) or die("mkdir($tmp_dir): $!\n");
    stash($args,'tmp_dir',$tmp_dir);
    chdir($tmp_dir) or die("cd $tmp_dir: $!\n");
    my $cmd = "git clone file://$repo_path $repo_name";
    run($args,$cmd,1);
    chdir($repo_name) or die("cd $tmp_dir/$repo_name: $!\n");
}

## copy_content - run the command to install this branch
##
sub copy_content {
    my($args) = @_;
    my $repo_name = get_arg($args,'repo');
    my $cmd = get_arg($args,"command",'');
    my $addl = join(' ',@{$args->{'_'}}) || '';
    if (!$cmd && (-f "Makefile")) {
        $cmd = "make $addl install";
    }
    if (!$cmd) {
        warn("# $W$P: $repo_name: null command - nothing done\n")
            if $VERBOSE;
    } else {
        warn("# $W$P: $repo_name: ==> $cmd\n")
            if $VERBOSE;
        run($args,$cmd,0);
    }
}

## cleanup_tmp - get rid of our tmp directory
##
sub cleanup_tmp {
    my($args) = @_;
    my $tmp_dir = stashed($args,'tmp_dir',undef);
    if (defined($tmp_dir) && !get_opt($args,'keep-tmp',0)) {
        my $cmd = "rm -rf $tmp_dir";
        run($args,$cmd,0);
    }
}

## afterwards - run any after-command that was specified
##
sub afterwards {
    my($args) = @_;
    my $cmd = get_arg($args,'after-command','');
    run($args,$cmd,0) if $cmd;
}

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    checkout_master($args);
    copy_content($args);
    afterwards($args);
    cleanup_tmp($args);
    exit(0);
}

__END__

=pod

=head1 NAME

push-content - push content from a git repo to its final destination

=head1 SYNOPSIS

  # check out the master branch of /data/repos/foo.com.git
  # into a temp directory and run "make install" in it
  $ push-content.pl -repo-root=/data/repos -repo=foo.com [args to make]

=head1 DESCRIPTION

This script is meant to be invoked from a git post-receive hook.  It
assumes we want the master branch of a git repository checked out and
installed.  Our strategy to do this is: check out the master branch
into a temp directory, cd into that directory and run C<make install>.

=head1 ARGUMENTS AND OPTIONS

Any non-optional arguments given to us are pasted between C<make> and
C<install> so you can e.g. set C<make> variables, e.g.

  push-content DSTDIR=/some/other/place

In addition to positional arguments for C<make> we accept the following
named options:

=over 4

=item -repo-root=dir

Set the root directory to search for git repositories.

=item -repo=name

Set the name of the git repository we want to process.  We are
reasonably smart and will use a directory named C<name.git> if it
exists and there is no directory named C<name> in the repo-root.

=item -tmpdir=dir

Use C<dir> as the base directory in which to create our temporary
checkout; by default it is C</tmp>.

=item -command=cmd

Use this command instead of C<make install>.  In this case positional
arguments are ignored, since you can just specify them as
C<-command="foo args">.

=item -after-command=cmd

Run this command after a successful installation.

=item -dry-run

=item -n

Do not actually run anything, just say what we would've done.

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
