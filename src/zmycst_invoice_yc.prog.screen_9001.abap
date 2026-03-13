PROCESS BEFORE OUTPUT.
*  MODULE status_0900.
*  MODULE mir7.
  CALL SUBSCREEN:subhead INCLUDING sy-repid '9101',
                 subitem INCLUDING sy-repid '9102'.

PROCESS AFTER INPUT.
  CALL SUBSCREEN:subhead,
                 subitem.
  MODULE user_command_9001.
