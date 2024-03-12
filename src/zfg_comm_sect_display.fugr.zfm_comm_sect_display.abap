FUNCTION zfm_comm_sect_display.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_COMMENT_SYSTEM) TYPE REF TO  ZIF_CMT_SYSTEM
*"     REFERENCE(I_SETTINGS) TYPE  ZIF_CMT_SYSTEM=>TS_DISPLAY OPTIONAL
*"----------------------------------------------------------------------

  CLEAR comment_system.
  comment_system = i_comment_system.
  CHECK comment_system IS BOUND.

  CLEAR settings.
  settings = i_settings.

  CALL SCREEN 100
    STARTING AT settings-start_col settings-start_row
    ENDING AT   settings-end_col   settings-end_row.


ENDFUNCTION.

MODULE pbo OUTPUT.

  SET TITLEBAR  `COMMENTS` WITH settings-title.
  SET PF-STATUS `POPUP_STATUS`.

  comment_system->dislplay_in_containers(
    section_container     = `C_SEC`
    textarea_container    = `C_NEW` ).

ENDMODULE.

MODULE pai INPUT.

  CLEAR save_ok.
  save_ok = ok_code.
  CLEAR ok_code.

  PERFORM uc_popup USING save_ok.

ENDMODULE.

FORM uc_popup USING p_ucomm TYPE sy-ucomm.

  CASE p_ucomm.
    WHEN `CANC`.
      comment_system->free( ).
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
