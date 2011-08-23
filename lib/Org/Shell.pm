=pod

=head1 NAME

Org::Shell - Convenient code for use from the shell

=head1 SYNOPSIS

  # search just the textual bodies of notes in foo.org for
  # the word "important"
  $ perl -MOrg::Shell -e grep_body '\bimportant\b' foo.org

  # try to do something sensible with foo.xml and produce
  # foo.org as a result
  $ perl -MOrg::Shell -e import_notes foo.xml

  # turn foo.org into a bunch of Tomboy-style XML files
  # with a .note extension
  $ perl -MOrg::Shell -e export_notes foo.org .note

=head1 DESCRIPTION

This module pollutes the main namespace with useful subs that do
sensible things with command-line arguments, but which can also be
called usefully from programs.

Of course I haven't written it yet, but soon it will be froody and
full of flavor.

=cut

package Org::Shell;
use strict;
use Carp;
require Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(grep_body grep_meta import_notes export_notes);
@EXPORT = @EXPORT_OK;

sub grep_body {
}

sub grep_meta {
}

sub import_notes {
}

sub export_notes {
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
