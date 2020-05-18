REPORT z_oo_abap_lvl02.
 
CLASS lcx_notif_exp DEFINITION INHERITING FROM cx_static_check .
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    ALIASES t100key FOR if_t100_message~t100key.
ENDCLASS.
 
CLASS lcl_notif_items DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_notif_no TYPE qmnum,
 
      read RETURNING VALUE(ro_notif_items) TYPE REF TO lcl_notif_items
           RAISING   lcx_notif_exp,
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
*    WRITE:/ 'Read notification items'.
 
    gt_notif_items = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
 
    RAISE EXCEPTION TYPE lcx_notif_exp MESSAGE e001(ziem_mwbf).
 
    ro_notif_items = me.
  ENDMETHOD.
 
 
  METHOD display.
    LOOP AT gt_notif_items[] ASSIGNING FIELD-SYMBOL(<lv_notif_item>).
      WRITE:/ |Item number { <lv_notif_item> }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
 
 
CLASS lcl_notification DEFINITION CREATE PROTECTED.
  PUBLIC SECTION.
    CONSTANTS:
      gc_internal_agent TYPE qmart VALUE 'IN',
      gc_external_agent TYPE qmart VALUE 'EX'.
    METHODS:
*      constructor
*        IMPORTING
*          iv_notif_no    TYPE qmnum
*          io_notif_items TYPE REF TO lcl_notif_items OPTIONAL,
 
      read RETURNING VALUE(ro_notification) TYPE REF TO lcl_notification
           RAISING   lcx_notif_exp,
      display.
 
  PROTECTED SECTION.
    DATA:
      gv_notif_no          TYPE qmnum,
      gv_agent             TYPE qmart,
      gv_notif_description TYPE string,
      go_notif_items       TYPE REF TO lcl_notif_items.
 
    METHODS:
      constructor
        IMPORTING
          iv_notif_no    TYPE qmnum
          io_notif_items TYPE REF TO lcl_notif_items OPTIONAL,
      do_smth
*        IMPORTING it_some_table TYPE ANY TABLE.
        IMPORTING VALUE(it_some_table) TYPE ANY TABLE.
 
  PRIVATE SECTION.
*    DATA gv_notif_description TYPE string.
 
ENDCLASS.
 
CLASS lcl_notification IMPLEMENTATION.
  METHOD constructor.
    gv_notif_no = iv_notif_no.
    go_notif_items = io_notif_items.
  ENDMETHOD.
 
  METHOD do_smth.
 
*    DATA(lt_the_same_table) = it_some_table.
*    SORT lt_the_same_table.
 
    SORT it_some_table.
 
  ENDMETHOD.
 
  METHOD read.
*    WRITE:/ 'Read notification head'.
 
    gv_notif_description = |Notification { gv_notif_no } description|.
 
    IF go_notif_items IS BOUND.
      go_notif_items->read( ).
    ENDIF.
 
    ro_notification = me.
  ENDMETHOD.
 
  METHOD display.
    WRITE:/ gv_notif_description.
 
    IF go_notif_items IS BOUND.
      go_notif_items->display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
 
CLASS lcl_notification_int_agent DEFINITION INHERITING FROM lcl_notification CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_notif_no    TYPE qmnum
          io_notif_items TYPE REF TO lcl_notif_items OPTIONAL,
      read REDEFINITION.
  PROTECTED SECTION.
    METHODS: do_smth REDEFINITION.
ENDCLASS.
 
CLASS lcl_notification_int_agent IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_notif_no = iv_notif_no io_notif_items = io_notif_items ).
    gv_agent = gc_internal_agent.
  ENDMETHOD.
 
  METHOD read.
    ro_notification = super->read( ).
 
    gv_notif_description = |Internal agent notification { gv_notif_no } description|.
  ENDMETHOD.
 
  METHOD do_smth.
  ENDMETHOD.
ENDCLASS.
 
CLASS lcl_notification_ext_agent DEFINITION INHERITING FROM lcl_notification CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_notif_no    TYPE qmnum
          io_notif_items TYPE REF TO lcl_notif_items OPTIONAL.
ENDCLASS.
 
CLASS lcl_notification_ext_agent IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_notif_no = iv_notif_no io_notif_items = io_notif_items ).
    gv_agent = gc_external_agent.
  ENDMETHOD.
ENDCLASS.
 
*   Factory method
CLASS lcl_notif_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        IMPORTING
                  iv_notif_no            TYPE qmnum
                  iv_agent               TYPE c
                  io_notif_items         TYPE REF TO lcl_notif_items OPTIONAL
        RETURNING VALUE(ro_notification) TYPE REF TO lcl_notification
        RAISING   lcx_notif_exp.
ENDCLASS.
 
CLASS lcl_notif_factory IMPLEMENTATION.
  METHOD  get_instance.
    ro_notification = SWITCH #( iv_agent
 
      WHEN lcl_notification=>gc_internal_agent
      THEN NEW lcl_notification_int_agent( iv_notif_no = iv_notif_no
        io_notif_items = io_notif_items )
 
      WHEN lcl_notification=>gc_external_agent
      THEN NEW lcl_notification_ext_agent( iv_notif_no = iv_notif_no
        io_notif_items = io_notif_items ) ).
 
    IF ro_notification IS NOT BOUND.
      RAISE EXCEPTION TYPE lcx_notif_exp MESSAGE e001(ziem_mwbf).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
 
CLASS lcl_temp DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA gv_type TYPE c.
    CLASS-METHODS: set_type IMPORTING iv_type TYPE c.
    CLASS-METHODS: class_constructor.
ENDCLASS.
 
CLASS lcl_temp IMPLEMENTATION.
  METHOD class_constructor.
    gv_type = 'A'.
  ENDMETHOD.
  METHOD set_type.
    gv_type = iv_type.
  ENDMETHOD.
ENDCLASS.
 
START-OF-SELECTION.
  CONSTANTS gc_notif_no TYPE qmnum VALUE '12345'.
 
  TRY  .
      lcl_notif_factory=>get_instance(
        iv_notif_no     = gc_notif_no
        iv_agent        = lcl_notification=>gc_external_agent
        io_notif_items  = NEW lcl_notif_items( gc_notif_no )
        )->read( )->display( ).
 
    CATCH lcx_notif_exp INTO DATA(lo_xexp).
 
*   Without Alias
*      lo_xexp->if_t100_message~t100key-msgid
*      lo_xexp->if_t100_message~t100key-msgno
 
*   With Alias
*      lo_xexp->t100key-msgid
 
      MESSAGE lo_xexp TYPE 'S' DISPLAY LIKE 'E'.
 
      WRITE lo_xexp->get_longtext( ).
 
*   Example with casting exception interfaces
    CATCH cx_static_check INTO DATA(lo_static_exp).
      DATA lo_t100 TYPE REF TO if_t100_message.
 
      TRY.
          lo_t100 ?= lo_static_exp.
          lo_t100->if_message~get_longtext( ).
 
          CAST if_t100_message( lo_static_exp )->if_message~get_longtext( ).
 
          CAST if_message( lo_static_exp )->get_longtext( ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
  ENDTRY.
 
  WRITE: lcl_temp=>gv_type.
  lcl_temp=>set_type( 'B' ).
  WRITE: lcl_temp=>gv_type.
