PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TABHEAD'
  MODULE tabhead_active_tab_set.
  CALL SUBSCREEN tabhead_sca
    INCLUDING g_tabhead-prog g_tabhead-subscreen.
*  MODULE status_0900.
*  MODULE mir7.

PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TABHEAD'
  CALL SUBSCREEN tabhead_sca.
  MODULE tabhead_active_tab_get.
*  MODULE user_command_0900.
