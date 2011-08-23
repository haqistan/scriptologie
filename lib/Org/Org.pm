=pod

=head1 NAME

Org - Emacs org-mode Perl Interface

=head1 SYNOPSIS

  use Org;

  my $org = Org->new({ file => "foo.org" })
    or die "Org parser failed on foo.org: $Org::ERROR";

  # spit out as web page
  print $org->to_html({ pretty => 1 });

  # spit out text re-indented
  print $org->to_text({ reindent => 1, indent => 4, lead => 8 });

  # search for notes that mention "Bombast" or "Bombay"
  my @matches = $org->search(qr/(?i)bomba(st|y)/);

  # search for notes with property Foo = 100
  my @matches =
    $org->search(
      { type => 'property', cmp => '=', name => 'Foo', val => 100 }
    );

  # return search result as independent Org container
  my $ref = $org->search({ ... });

  # now we can operate on the artificial container:
  print $ref->to_html();

  # list property names (in :PROPERTIES:)
  my @props = $org->properties();

  # list property names from a random drawer
  my @props = $org->properties("someDrawer");

  # get, set properties
  use Date::Parse;
  my $val = $org->property('time-created');
  my $t_future = str2time($val) + 3600;
  $org->property('time-next-read' => $org->format_time($t_future));


=head1 DESCRIPTION

This module presents an OO interface the the Emacs org-mode text
format.  It consists of a parser for parsing existing org-mode content
into our internal data structures, methods for turning this internal
data structure into a variety of output formats (include org-mode
itself again), and utilities for querying and modifying the internal
represention, including a powerful C<search> method.

=head2 API

The C<Org> class and its instances support the following methods.

=cut

package Org;
use strict;
use Carp;
use Org::Ord;
use base qw(Class::Data::Inheritable Class::Accessor);
use overload '""' => \&to_string;

__PACKAGE__->mk_accessors(qw(parent kids drawers title));

=pod

=head2 $instance = Org->new($hashref)

Construct a new Org-mode object.  If we are not given any arguments,
the returned object is empty and untitled.  The following
parameters can be specified in C<$hashref>:

=over 4

=item file => $filename

Initialize our contents from the named file.  The type of file is
sussed out from its extension, if any; however, currently only
org-mode text files are supported.

=item contents => $string

Initialize our contents from the content in the given string or
C<IO::String> object.

=item parent => $org

Specify a parent (container) Org object; otherwise we return a root.

=back

=cut

sub new {
    my $class = shift;
    $class = ref($class) || $class;
    my $params;
    if (@_ == 1) {
        ($params) = @_;
        die("$class: new: no ref!?") unless ref($params);
    } elsif (@_ > 1) {
        warn("$class: new: odd args: @_") if (scalar(@_) & 1);
        $params = { @_ };
    }
    my $ref = $class->SUPER::new($params);
    return $ref->init($params);
}

sub init {
    my($self,$params) = @_;
    my $class = ref($self);
    my $tmp = $self->parent;
    die("$class: invalid parent $tmp") if (defined($tmp) && $class->IsA($tmp));
    $self->kids([]) unless defined($self->kids);
    $self->drawers({}) unless defined($self->drawers);
    $self->title("") unless defined($self->title);
    if ($params->{'file'}) {
        $self->parse_file($params->{'file'});
    } elsif ($params->{'content'}) {
        $self->parse_string($params->{'content'});
    }
    return $self;
}

############################################################################
#
# Exported API
#
############################################################################

sub parse_file {
}

sub parse_string {
}

sub add {
}

sub remove {
}

sub children {
}

sub properties {
}

sub property {
}

sub search {
}

=pod

=head2 $string = $org->to_org({ ... options });

=cut

sub to_org { shift->_convert('org',@_); }

sub to_html { shift->_convert('html',@_); }

sub to_text { shift->_convert('text',@_); }

sub to_xml { shift->_convert('xml',@_); }

sub format_time {
    my($self,$t) = @_;
}

############################################################################
#
# Internal Methods
#
############################################################################

sub _drawer {
    my($self,$drawer) = @_;
    my $d = $self->drawers->{$drawer}
        if exists($self->drawers->{$drawer});
    unless ($d) {
        $d = Org::Ord->new();
        $self->drawers->{$drawer} = $d;
    }
    return $d;
}

sub _prop {
    my($self,$name,$val) = @_;
    my $have_val = (@_ > 2) ? 1 : 0;
    my $p = $self->_drawer($D_PROPERTIES);
    my $old = $p->get($name);
    $p->set($name => $val) if $have_val;
    return $old;
}

sub _parse {
}

sub _convert {
}

1;

__END__

=pod

=head1 SEE ALSO

We live in splendid isolation.

=head1 AUTHOR

  attila <attila@stalphonsos.com> # keyid 0x4FFCBB9C

=head1 COPYRIGHT AND LICENSE

  (C) 2002-2011 by attila <attila@stalphonsos.com>
  All Rights Reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
  
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following
        disclaimer in the documentation and/or other materials provided
        with the distribution.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# cperl-indent-level: 4
# cperl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# time-stamp-line-limit: 40
# End:
##
