INTERFACE zif_cmt_system
  PUBLIC.


  TYPES:
    BEGIN OF ts_display,
      start_col TYPE i,
      start_row TYPE i,
      end_col   TYPE i,
      end_row   TYPE i,
      title     TYPE string,
    END OF ts_display.

  METHODS render_textarea
    RETURNING VALUE(result) TYPE zif_cmt_manager=>tt_html_data.

  METHODS render_comments
    RETURNING VALUE(result) TYPE zif_cmt_manager=>tt_html_data.

  METHODS add_comment
    IMPORTING !text TYPE clike.

  METHODS dislplay_in_containers
    IMPORTING section_container  TYPE clike
              textarea_container TYPE clike.

  METHODS display
    IMPORTING settings TYPE ts_display OPTIONAL.

  METHODS like_comment
    IMPORTING comment_id TYPE sysuuid_c26.

  METHODS dislike_comment
    IMPORTING comment_id TYPE sysuuid_c26.

  METHODS delete_comment
    IMPORTING comment_id TYPE sysuuid_c26.

  METHODS free.
  METHODS load_theme.

  METHODS mark_as_read
    CHANGING !comments TYPE zif_cmt_manager=>tt_comments.

  METHODS notify_to_read
    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.
