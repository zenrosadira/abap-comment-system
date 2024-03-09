FUNCTION-POOL ZFG_COMM_SECT_DISPLAY.           "MESSAGE-ID ..

DATA comment_system TYPE REF TO zif_cmt_system.
DATA settings       TYPE zif_cmt_system=>ts_display.
DATA ok_code        LIKE sy-ucomm.
DATA save_ok        LIKE sy-ucomm.
* INCLUDE LZFG_TBOX_SMART_ALVD...            " Local class definition
