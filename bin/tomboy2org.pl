#!/usr/bin/perl
##
# tomboy2org.pl - convert tomboy-style XML notes into org-mode
#
# Time-stamp: <2011-08-01 11:51:29 snl@stalphonsos.com>
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
use XML::Twig;
use Sys::Hostname;
#use Data::Dumper;
use Text::Format;
use IO::File;
use POSIX qw();
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
    pod2usage(-verbose => 2)            if $::VERBOSE && !defined($msg);
    if (defined($msg)) {
        print STDERR "$::P: $msg\n"     if defined $msg;
    } else {
        print STDERR "$::P: convert tomboy-style XML notes into org-mode\n";
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

##

## find_notes - find note files in a directory
##
sub find_notes {
    my($args,$dir) = @_;
    opendir(DIR,$dir) or die("$P: opendir($dir): $!\n");
    my @all = (
        grep { ($_ !~ /^\./) && (($_ =~ /\.(xml|note)$/) || (-d "$dir/$_")) }
        readdir(DIR)
    );
    closedir(DIR);
    warn("; searching: $dir (".scalar(@all).")\n") if $VERBOSE;
    my @subdirs;
    my @notes;
    foreach (@all) {
        my $fn = "$dir/$_";
        if (-d $fn) {
            push(@subdirs,$fn);
        } else {
            push(@notes,$fn);
        }
    }
    if ($args->{'recurse'}) {
        push(@notes,find_notes($args,$_)) foreach (@subdirs);
    }
    return @notes;
}

## get_formatter - cons up or return the Text::Format object we're using
##
sub get_formatter {
    my($args) = @_;
    my $fmt = $args->{' fmt'};
    unless ($fmt) {
        my $cols = $args->{'columns'} || 72;
        my $tabs = $args->{'tabstop'} || 2;
        my $first = $args->{'indent'} || 0;
        $fmt = Text::Format->new();
        $fmt->columns($cols);
        $fmt->tabstop($tabs);
        $fmt->firstIndent($first);
        $args->{' fmt'} = $fmt;
    }
    return $fmt;
}

## mung_text - reformat paragraphy text
##
sub mung_text {
    my($args,$string) = @_;
    my $fmt = get_formatter($args);
    my $nnl = $args->{'inter-paragraph'} || 1;
    my $psep = '';
    $psep = "\n" x $nnl if $nnl > 1;
    return join($psep,$fmt->paragraphs(split(/\n/,$string)));
}

## file_note - stash away a note we've parsed
##
sub file_note {
    my($args,$notebook,$string) = @_;
    my $defn = $args->{'default-notebook'} || 'notes';
    my $org_name = $notebook || $defn;
#    my $org_name = $args->{'notebooks'} ? $notebook : $defn;
    if (!defined($args->{' orgs'}->{$org_name})) {
        $args->{' ordered'} ||= [];
        push(@{$args->{' ordered'}},$org_name);
        $args->{' orgs'}->{$org_name} = [];
    }
    push(@{$args->{' orgs'}->{$org_name}},$string);
    $args->{' count'} ||= 0;
    ++$args->{' count'};
}

## note_to_org - parse a note XML file into our internal representation
##
sub note_to_org {
    my($args,$filename) = @_;
    my $twig = XML::Twig->new();
    $twig->parsefile($filename);
    my @org = ();
    my $root = $twig->root;
    my($title) = $root->children('title');
    die("$filename: no title\n") unless $title;
    my($text) = $root->children('text');
    die("$filename: no text\n") unless $text;
    my $ttl = $title->text;
    my $txt = mung_text($args,$text->text);
    my $host = hostname;
    my($notebook) = (
        map {
            my $t = $_->text;
            my @p = split(/:/,$t);
            pop(@p);
        }
        grep { defined }
        $root->children('tags')
    );
    $notebook ||= $args->{'default-notebook'} || 'notes';
    my $tfmt = $args->{'tstamp-fmt'} || '%Y-%m-%d %H:%M:%S %z';
    my $tstamp = POSIX::strftime($tfmt,localtime(time));
    my $props =
        join(
            "\n",
            ":TAGS:$notebook",
            ":original-filename:$filename",
            ":original-hostname:$host",
            ":import-time:$tstamp",
            map { sprintf(":%s:%s",$_->tag,$_->text) }
            grep { defined }
            map { $root->children($_) }
            qw(last-change-date last-metadata-change-date create-date)
        );
    my $leader = defined($args->{'notebooks'}) ? '*' : '**';
    my $str = "$leader $ttl\n";
    $str .= ":PROPERTIES:\n$props\n:END:\n";
    $str .= "$txt\n";
    file_note($args,$notebook,$str);
}

## get_org_names - get all of the notebook names we've seen
##
sub get_org_names {
    my($args) = @_;
    return @{$args->{' ordered'}};
}

## get_org_notes - get the notes associated with a notebook
##
sub get_org_notes {
    my($args,$name) = @_;
    my @notes = ();
    if (defined($args->{' orgs'}->{$name})) {
        @notes = @{$args->{' orgs'}->{$name}};
    }
    return @notes;
}

## open_org_with_backup - open an org-mode output file, possibly backing it up
##
sub open_org_with_backup {
    my($args,$name) = @_;
    my $dir = $args->{'dir'} || '.';
    $name =~ s/\s/_/gs;
    my $fn = $name . ".org";
    unless ($args->{'overwrite'}) {
        my $count = 0;
        while (-f $fn) {
            ++$count;
            $fn = $name . "." . sprintf("%03d",$count) . ".org";
        }
    }
    my $path = "$dir/$fn";
#    warn("; $name => $path\n") if $VERBOSE;
    my $fh = IO::File->new("> $path") or die("$P: $path: $!\n");
    binmode($fh,':utf8');
    ## XXX make this customizable via a template file or something...
    $fh->print("# -*- mode:org; indent-tabs-mode:nil; tab-width:2 -*-\n\n");
    return($fh,$fn);
}

## flush_output - write out any accumulated notes in org format
##
sub flush_output {
    my($args) = @_;
    my($nnotes,$norgs) = ($args->{' count'},scalar(keys(%{$args->{' orgs'}})));
    my @orgs = get_org_names($args);
    if ($args->{'notebooks'}) {
        warn("; spitting out $norgs separate org files...\n")
            if $VERBOSE;
        foreach my $org (@orgs) {
            my @notes = get_org_notes($args,$org);
            if (!@notes) {
                warn("; $org - no notes - skipped !?\n");
            } else {
                my($fh,$fn) = open_org_with_backup($args,$org);
                $fh->print("$_\n") foreach (@notes);
                warn("; $org - wrote ".scalar(@notes)." notes => $fn\n")
                    if $VERBOSE;
                $fh->close();
            }
        }
    } else {
        my $out = $args->{'output'} || $args->{'default-notebook'} || 'notes';
        warn("; spitting out a single org file: ${out}.org ...\n")
            if $VERBOSE;
        my($fh,$fn) = open_org_with_backup($args,$out);
        my $total = 0;
        foreach my $org (sort { $a cmp $b } @orgs) {
            my @notes = get_org_notes($args,$org);
            $fh->print("* $org\n\n");
            $fh->print("$_\n") foreach (@notes);
            $total += scalar(@notes);
        }
        warn("; $out - wrote $total notes\n") if $VERBOSE;
        $fh->close();
    }
}

##
# Main program
##

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    if (!@{$args->{'_'}}) {
        my $home = $ENV{'HOME'} || '';
        my($dir) = grep { -d $_ } (
            "${home}/.local/share/gnote",
            "${home}/.gnote",
            "${home}/.local/share/tomboy",
            "${home}/.tomboy"
        );
        usage("no files or directories given and no default found")
            unless $dir;
        $args->{'_'} = [ $dir ];
        warn("; assuming input directory: $dir\n") if $VERBOSE;
    }
    foreach my $a (@{$args->{'_'}}) {
        my @notes = ();
        my $in;
        my $notes;
        if (-d $a) {
            push(@notes,find_notes($args,$a));
            $in = 'under';
            $notes = 'notes';
        } elsif (-f $a) {
            push(@notes,$a);
            $in  = 'in';
            $notes = 'note';
        } else {
            die("$P: $a ?\n");
        }
        warn("; processing ".scalar(@notes)." $notes $in $a ...\n")
            if $VERBOSE;
        note_to_org($args,$_) foreach (@notes);
    }
    flush_output($args);
    exit(0);
}

__END__

=pod

=head1 NAME

tomboy2org.pl - convert tomboy-style XML notes into emacs org-mode

=head1 SYNOPSIS

  # turn your whole pile of gnotes into one note.org file
  $ tomboy2org.pl -verbose
  ; assuming input directory: /home/you/.local/share/gnote
  ; searching: /home/you/.local/share/gnote (485)
  ; processing 483 notes under /home/you/.local/share/gnote ...
  ; spitting out a single org file: notes.org ...
  ; notes - wrote 483 notes

=head1 DESCRIPTION

We use L<XML::Twig> to parse Tomboy's XML-based note file format and
dump the results in whatever way we are instructed.  We can do
directory trees recursively or single files.  The author uses GNote, a
re-write of Tomboy Notes in C++, so Tomboy users might have to make
some adjustments if there are differences between the underlying XML
storage formats used in the two variants.  It does not appear that there
are any differences, since GNote claims to be a feature-for-feature
rewrite of the C# program into C++.

Our non-optional arguments should be paths to directories or files
containing notes.  If we scan directories, we look for files that end
in C<.note> or C<.xml>.  We process our arguments in turn and parse
all notes we find before producing output.  If no arguments are given
then we search a default list of places and stop at the first one; our
search list is biased towards GNote.

=head2 Notebooks

If the C<-notebooks> option is not specified all notes we encounter
will be packed into a single org-mode file.  In this case we will use
any notebook tag associated with the note to group it under a
first-level heading named as per the notebook, with each note in that
notebook as a second-level heading underneath it.  If C<-notebooks> is
specified then we instead spit out one org-mode file per notebook, and
each note is a first-level heading in that file.

=head2 Properties

We pull out many of the properties associated with notes in this
format, such as the various dates they were created, modified, etc.
Org-mode gives us the general notion of C<drawers> with the specific
drawer named C<PROPERTIES> available for storage of arbitrary
attributes that can be easily munged via Elisp.  We store the Tomboy
properties with their original names: C<last-change-date>,
C<last-metadat-change-date>, and C<create-date>.  The format of these
date/times is ISO but I have a feeling another format would be more
amenable to Elisp.

In addition to these properties, we store the name of the notebook in
the org-mode standard C<TAGS> property, and add two attributes of our
own: C<original-filename>, C<original-host>, and C<import-time>.  The
first two are the full path to the input XML file and the name of the
host as reported by L<Sys::Hostname>; the last is the date and time
that this script was run.  It seemed like the sane thing to do.

=head1 OPTIONS

We accept the following optionology:

=over 4

=item -recurse

Recursively descend into directories we are given.  Off by default (to
avoid descending into the C<Backup> directory that GNote creates by
default).

=item -default-notebook=def

Name of the default notebook/org file to produce; defaults to C<notes>.

=item -dir=dir

Directory to drop our output into; defaults to the current directory.

=item -notebooks

Produce one C<.org> file per notebook as seen in the tags in the notes
files themselves.  For instance if there are notebooks named C<Red>,
C<Blue> and C<Green> then we will spit org-formatted notes into
C<Red.org>, C<Blue.org> and C<Green.org>, with notes that are uncategorized
landing in the default notebook (C<notes.org>).

=item -columns=int

Column at which we will wrap text; default is 72.

=item -tabstop=int

Tab-stop position; default is 2.

=item -indent=int

Indent for first line in paragraph; default is zero.

=item -inter-paragraph=int

Number of blank lines between paragraphs of text in an entry.
Defaults to 1.

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

=head1 BUGS / TODO

=over 4

=item *
Improve attribute handling... at least make it more generic and configurable.

=item *
Suss out the date/time format issue for the Tomboy timestamp attributes and
mung them during conversion so they are easy to use in Emacs and org-mode.

=item *
Provide a way for supplying a template / starting-point org file.

=item *
Make an attempt to suss out if the input comes from GNote or Tomboy and
mark it up as such via properties.

=back

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   31 Jul 11     attila  Written

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
