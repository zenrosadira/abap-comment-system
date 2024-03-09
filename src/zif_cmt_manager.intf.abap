INTERFACE zif_cmt_manager
  PUBLIC.


  TYPES:
    BEGIN OF ts_user_name,
      user_id   TYPE c LENGTH 12,
      user_name TYPE string,
    END OF ts_user_name.
  TYPES tt_users_name TYPE TABLE OF ts_user_name WITH EMPTY KEY.
  TYPES:
    BEGIN OF ts_comment,
      comment_id     TYPE sysuuid_c26,
      comment_author TYPE c LENGTH 12,
      comment_date   TYPE d,
      comment_time   TYPE t,
      comment_text   TYPE xstring,
      user_upvoting  TYPE TABLE OF char12 WITH EMPTY KEY,
      read_by        TYPE TABLE OF char12 WITH EMPTY KEY,
    END OF ts_comment.
  TYPES tt_comments    TYPE TABLE OF ts_comment WITH EMPTY KEY.
  TYPES tt_db_comments TYPE TABLE OF zcmt_comment_t WITH EMPTY KEY.
  TYPES tt_db_upvotes  TYPE TABLE OF zcmt_upvotes_t WITH EMPTY KEY.
  TYPES tt_db_read_by  TYPE TABLE OF zcmt_comread_t WITH EMPTY KEY.
  TYPES:
    BEGIN OF ts_html_data,
      html TYPE c LENGTH 1000,
    END OF ts_html_data.
  TYPES tt_html_data TYPE TABLE OF ts_html_data WITH EMPTY KEY.

  METHODS get_user_full_name
    IMPORTING user_id       TYPE clike
    RETURNING VALUE(result) TYPE string.

  METHODS create_id
    RETURNING VALUE(result) TYPE sysuuid_c26.

  METHODS load_upvotes
    IMPORTING !comments     TYPE tt_db_comments
    RETURNING VALUE(result) TYPE tt_db_upvotes.

  METHODS load_readby
    IMPORTING !comments     TYPE tt_db_comments
    RETURNING VALUE(result) TYPE tt_db_read_by.

  METHODS load_comments
    IMPORTING section_id    TYPE clike
    RETURNING VALUE(result) TYPE tt_db_comments.
ENDINTERFACE.
