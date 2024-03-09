INTERFACE zif_cmt_theme
  PUBLIC.


  METHODS render_comments
    IMPORTING !comments     TYPE zif_cmt_manager=>tt_comments
    RETURNING VALUE(result) TYPE zif_cmt_manager=>tt_html_data.

  METHODS render_textarea
    RETURNING VALUE(result) TYPE zif_cmt_manager=>tt_html_data.
ENDINTERFACE.
