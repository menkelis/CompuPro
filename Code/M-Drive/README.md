The driver looked like it loaded correctly.
The program MFORM.COM found the driver and said it formatted
the drive.
Trying to do a DIR command just returns 'Bad Sector'.
I traced the code with DDT, and a simple read sector program
and that looked good, just returned random garbage.
Then I made a quick program to fill track 0, sector 0 with
the pattern '0E5' and was able also read that sector back just fine.
Maybe some other eye's on the code will point what I missed
before I can find the problem myself.

Next program I am writing will be a memory test for the card.
