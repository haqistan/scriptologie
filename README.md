# Scriptologie - because somtimes, you just need a script #

These are scripts from my bag of tricks that I've found useful over
time.  Some of them are from a long time ago and my coding style has
changed over time, so they are a bit uneven stylistically.

Feel free to bug me about them.  They are all released under an ISC
license, present in this repo.

I keep libraries in `lib` and scripts in `scripts`.  All Perl sources have
PODs of some kind.  Libraries generally use Makefile.PL.  I will
probably change all of this over time.

Scripts include:

* tomboy2org.pl - Translate Tomboy/GNote-style XML notes into org-mode
* post-receive-* - Git post-receive hooks
* push-content.pl - Git-aware content "push" from post-receive script
* overcopy.pl - carefully copy from one tree to another
* diffsplit.pl - split the output of git-diff into OpenBSD port patches/
* diffscend.pl - turn the diffs between two trees into OBSD patches/
* djb2zone - translate djbdns-style data into BIND-style zones

Libraries include:

* Org - Native Perl interface to the Emacs org-mode text file format
--
attila@stalphonsos.com | 0xE6CC1EDB | hack("free") or die;
