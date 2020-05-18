REPORT z_oo_abap_lvl01.
 
CLASS lcl_notif_items DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_notif_no TYPE qmnum OPTIONAL,
 
      read RETURNING VALUE(ro_notif_items) TYPE REF TO lcl_notif_items,
      display.
 
  PRIVATE SECTION.
    DATA:
      gv_notif_no    TYPE qmnum,
      gt_notif_items TYPE STANDARD TABLE OF i.
ENDCLASS.
 
CLASS lcl_notif_items IMPLEMENTATION.
  METHOD constructor.
    gv_notif_no = iv_notif_no.
  ENDMETHOD.
 
  METHOD read.
    gt_notif_items = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
 
    ro_notif_items = me.
  ENDMETHOD.
 
  METHOD display.
    LOOP AT gt_notif_items[] ASSIGNING FIELD-SYMBOL(<lv_notif_item>).
      WRITE:/ |Item number { <lv_notif_item> }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
 
 
CLASS lcl_notification DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_notification,
        notif_no    TYPE qmnum,
        type        TYPE qmart,
        description TYPE string,
      END OF ts_notification.
 
    METHODS:
      constructor
        IMPORTING
          iv_notif_no     TYPE qmnum OPTIONAL
          is_notification TYPE ts_notification OPTIONAL
          io_notif_items  TYPE REF TO lcl_notif_items OPTIONAL,
 
      read RETURNING VALUE(ro_notification) TYPE REF TO lcl_notification,
      save RETURNING VALUE(ro_notification) TYPE REF TO lcl_notification,
      is_created RETURNING VALUE(lv_created) TYPE abap_bool,
      set_description IMPORTING iv_description TYPE string,
      display.
 
  PRIVATE SECTION.
    DATA:
      gs_notification TYPE ts_notification,
      go_notif_items  TYPE REF TO lcl_notif_items.
ENDCLASS.
 
CLASS lcl_notification IMPLEMENTATION.
  METHOD constructor.
    IF iv_notif_no IS SUPPLIED.
      gs_notification-notif_no = iv_notif_no.
    ELSE.
      gs_notification = is_notification.
    ENDIF.
 
    go_notif_items = io_notif_items.
  ENDMETHOD.
 
  METHOD read.
*   Get data from DB
    gs_notification-type = 'A1'.
    gs_notification-description = |Notification { gs_notification-notif_no } description|.
 
    go_notif_items->read( ).
    ro_notification = me.
  ENDMETHOD.
 
 
  METHOD save.
*    Save data to DB witn new notif number
    gs_notification-notif_no = cl_abap_random_int=>create( )->get_next( ).
 
    ro_notification = me.
  ENDMETHOD.
 
  METHOD set_description.
    gs_notification-description = iv_description.
  ENDMETHOD.
 
  METHOD display.
    WRITE:/ gs_notification-description.
 
    IF go_notif_items IS BOUND.
      go_notif_items->display( ).
    ENDIF.
  ENDMETHOD.
 
  METHOD is_created.
    lv_created = xsdbool( gs_notification-notif_no IS INITIAL ).
  ENDMETHOD.
ENDCLASS.
 
START-OF-SELECTION.
  CONSTANTS gc_notif_no TYPE qmnum VALUE '12345'.
 
  NEW lcl_notification(
    iv_notif_no = gc_notif_no
    io_notif_items = NEW lcl_notif_items( gc_notif_no )
  )->read( )->display( ).
