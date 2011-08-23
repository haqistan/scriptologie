=pod

=head1 NAME

Org::Ord - Ordered Set

=head1 SYNOPSIS

  use Org::Ord;

  $ord = Ord::Ord->new();

  $ord->add($obj)->add($another_obj);

  $n = $ord->length;

  @kids = $ord->children;

=head1 DESCRIPTION

An ordered set of Org-y things.

=cut

package Org::Ord;
use strict;
use Carp;
use base qw(Class::Acessor);


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
    return $self;
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
