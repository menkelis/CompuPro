!  9"R1��(�8�T́���d�w��j�N!  x��6 #�.X:] � �N:m � �N!l T�;! ͔"o!T\ �;! ͔�:p��N:o��N"m�!B\ �;! ͔"k�����!e > �«6C#6O#6M:] �0���*] "b !MD"] !IS"_ >K2a �2| \ � �<�N�� ��� \ � ������F�2j *mDM"d"f! :j2j���':j�2j{��=:j*dw#"d�2j�#x��*d"h �F }��\$�o+"o�*f �F"*m"�*k��ͯ*m3ͯ*f�*h�FH;ͯ*o#bPï�zͿ{ͿPY	� ���0�:���w#���0�:���w#�!2\ �;\ � \ � �<�N�2| ! ��� \ � ��N*o��"o�2�� ��\ � ~#�=�}�o|�g��	� *R��
G-MDRIVE v1.0 - Make MDRIVE.COM

Creates the program MDRIVE.COM

Usage: G-MDRIVE param1 param2
  Where param1 & param2 = [filename]XX[.ext]

param1 and param2 identify two MDISK.COM image files
of two different memory sizes

Examples:
  G-MDRIVE MDISK000.COM MDISK100.COM
  G-MDRIVE MDISK0 MDISK1  (assumes .COM extension)
  G-MDRIVE 000 100        (assumes filename like MDISKxx.COM)
$MDRIVE module size: xxxxh
$MDISK  module start: xxxxh
$MDISK  module size: xxxxh
$BITMAP size: xxxxh
$Final image size: xxxxh
$
MDRIVE.COM Created
$
File sizes do not match
$
File not found
$
File read error
$
The two files are the same MDISK memory size
$
Can't create output file
$
File write error, disk full?
$ MDRIVE  COM     L-MDRIVECOM                                                  