PROCESS BEFORE OUTPUT.
  MODULE pbo_9202.
*
PROCESS AFTER INPUT.
  MODULE pai_9202.

PROCESS ON VALUE-REQUEST.
*  FIELD ls_headerdata-diff_inv MODULE f4_lifnr.
  FIELD ls_headerdata-doc_type MODULE f4_blart.
