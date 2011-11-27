#!/usr/bin/perl
##
# push-content.pl - push a new checkin into the web root
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
use vars qw($P $COPY_YEARS $VERBOSE $DEFAULTS $VERSION);

BEGIN {
    ($P) = reverse(split('/', $0)); # XXX File::Spec would be better
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
        print STDERR "$::P: push content into a vhost\n";
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
            warn("# dry-run: $cmd\n")                           if $VERBOSE;
        } else {
            warn("# run: $cmd\n")                               if $VERBOSE;
            system($cmd) == 0
                or die("$cmd: $!\n");
        }
    }
}


sub checkout_master {
    my($args) = @_;
    my $repo_root = get_arg($args,'repo-root','/data/repo/git');
    my $repo_name = get_arg($args,'repo','cluefactory.com');
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

sub meta_data_differs {
    my($a,$b) = @_;
    my $ma = $a->[9];
    my $mb = $a->[9];
    my $diff = 0;
    if (defined($ma) && defined($mb)) {
        $diff = ($ma != $mb) ? 1 : 0;
    } elsif (!defined($ma) && !defined($mb)) {
        $diff = 0;
    } else {
        $diff = 1;
    }
    return $diff;
}

sub copy_content {
    my($args) = @_;
    my $repo_name = get_arg($args,'repo','cluefactory.com');
    my $cmd = get_arg($args,"command",'');
    my $vhost_root = get_arg($args,'vhost-root','/data/web/vhosts');
    my $vhost_name = get_arg($args,'vhost-name',$repo_name);
    my $vhost_dir = $vhost_root . "/${vhost_name}";
    die("$0: vhost_dir $vhost_dir does not exists for $vhost_name\n")
        unless (-d $vhost_dir);
    my $vhost_conf_s = get_arg($args,'vhost-conf-subdir','conf');
    my $vhost_conf_d = $vhost_dir . "/${vhost_conf_s}";
    my $vhost_conf_n = get_arg($args,'vhost-conf-file','httpd.conf');
    my $vhost_conf_f = "${vhost_conf_d}/${vhost_conf_n}";
    my $vhost_conf_b = undef;
    if (-f $vhost_conf_f) {
        $vhost_conf_b = [ stat($vhost_conf_f) ];
    }
    my $need_restart = 0;
    if (-f "Makefile") {
        $cmd = "make VHOST_ROOT=${vhost_root} VHOST_NAME=${vhost_name} VHOST_DIR=${vhost_dir} install";
    } elsif (-x "./install.pl") {
        $cmd = "./install.pl ${vhost_dir} ${vhost_name} ${vhost_root}";
    } elsif (-x "./install.sh") {
        $cmd = "./install.sh ${vhost_dir} ${vhost_name} ${vhost_root}";
    }
    if (!$cmd) {
        warn("# $repo_name: null command - nothing done\n")     if $VERBOSE;
    } else {
        warn("# $repo_name: ==> $cmd\n")                        if $VERBOSE;
        run($args,$cmd,0);
        if ($vhost_conf_b) {
            my $vhost_conf_a = [ stat($vhost_conf_f) ];
            $need_restart = meta_data_differs($vhost_conf_b,$vhost_conf_a);
            warn("# restart checking on $vhost_conf_f => $need_restart\n")
                                                                if $VERBOSE;
        }
    }
    return $need_restart;
}

sub restart_apache {
    my($args) = @_;
    my $cmd = get_arg($args,'restart-command','sudo apachectl restart');
    run($args,$cmd,0);
}

sub cleanup_tmp {
    my($args) = @_;
    my $tmp_dir = stashed($args,'tmp_dir',undef);
    if (defined($tmp_dir) && !get_opt($args,'keep-tmp',0)) {
        my $cmd = "rm -rf $tmp_dir";
        run($args,$cmd,0);
    }
}

##

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    checkout_master($args);
    restart_apache($args) if copy_content($args);
    cleanup_tmp($args);
    exit(0);
}

__END__

=pod

program - purpose

=head1 SYNOPSIS

  # clever comment
  $ example command

=head1 DESCRIPTION

This program does it all.

=head1 OPTIONS

We accept the following optionology:

=over 4

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

  0.1.0   16 Feb 07     attila  Started

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
