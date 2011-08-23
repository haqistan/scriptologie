## Org.pm - Emacs org-mode interface in Perl

use ExtUtils::MakeMaker;
eval(`grep VERSION= Org.pm|sed -e 's,VERSION=,Org_VERSION=,'`);

print STDERR "Welcome to Org $Org_VERSION\n\n";

WriteMakefile(
    AUTHOR => 'attila <attila@stalphonsos.com>',
    ABSTRACT => 'Emacs org-mode interface in Perl',
    NAME => 'Org',
    VERSION_FROM => 'Org.pm',
    PREREQ_PM => {
	'IO::File' => 0,
	'IO::String' => 0,
	'Class::Data::Inheritable' => 0,
	'Class::Accessor' => 0,
    },
    PM => {
	'Org.pm' => '$(INST_LIBDIR)/Org.pm',
	'Ord.pm' => '$(INST_LIBDIR)/Org/Ord.pm',
	'Shell.pm' => '$(INST_LIBDIR)/Org/Shell.pm',
    }
);
sub MY::postamble {
    print STDERR "To install completely:\n";
    print STDERR "    \$ make\n    \$ sudo make install\n";
    print STDERR "\nDon't forget to read the README and the POD.\n";
    return <<__MyTaRgEt__;
OPENSSLCMD?=openssl
MD5CMD?=\$(OPENSSLCMD) dgst -md5
SHA1CMD?=\$(OPENSSLCMD) dgst -sha1
RIPEMD160CMD?=\$(OPENSSLCMD) dgst -ripemd160
GPGCMD?=gpg
GPGKEYID?=0x4FFCBB9C
RELEASEDIR?=\$\$HOME/release
RELEASETARBALL?=\$(NAME)-\$(VERSION).tar.gz
RELEASEFILES?=\$(RELEASETARBALL) \$(NAME)-\$(VERSION).tar.gz.md5 \$(NAME)-\$(VERSION).tar.gz.sha1 \$(NAME)-\$(VERSION).tar.gz.ripemd160 \$(NAME)-\$(VERSION).tar.gz.asc
RELEASETARGETS?=dist md5 sha1 ripemd160 asc
RM?=/bin/rm
SHAREDIR?=\$(PREFIX)/share
EXAMPLEDIR?=\$(SHAREDIR)/examples/\$(NAME)
EXAMPLES?=eg
MKDIR_P?=mkdir -p

clean::
	\$(RM) -f *.flc *.bak *~ .tec*

force-releasefiles: cleanrelease releasefiles

distclean:: cleanrelease

install :: \$(EXAMPLEDIR)
	(cd \$(EXAMPLES); \$(TAR) cf - `grep eg/ ../MANIFEST | sed -e 's|eg/||'`) | (cd \$(EXAMPLEDIR); \$(TAR) xf -)

\$(EXAMPLEDIR) :
	\$(MKDIR_P) \$(EXAMPLEDIR)

cleanrelease:
	-\$(RM) -f \$(RELEASEFILES)

releasefiles: \$(RELEASETARGETS)

release: releasefiles
	\@-[ -d \$(RELEASEDIR) ] && \$(MV) \$(RELEASEFILES) \$(RELEASEDIR)
	\@echo Moved release files to \$(RELEASEDIR): \$(RELEASEFILES)

md5: \$(RELEASETARBALL)
	\$(MD5CMD) \$(NAME)-\$(VERSION).tar.gz > \$(NAME)-\$(VERSION).tar.gz.md5

sha1: \$(RELEASETARBALL)
	\$(SHA1CMD) \$(NAME)-\$(VERSION).tar.gz > \$(NAME)-\$(VERSION).tar.gz.sha1

ripemd160: \$(RELEASETARBALL)
	\$(RIPEMD160CMD) \$(NAME)-\$(VERSION).tar.gz > \$(NAME)-\$(VERSION).tar.gz.ripemd160

asc: \$(RELEASETARBALL)
	\$(GPGCMD) --sign --default-key \$(GPGKEYID) --detach --armor \$(NAME)-\$(VERSION).tar.gz
__MyTaRgEt__
}
