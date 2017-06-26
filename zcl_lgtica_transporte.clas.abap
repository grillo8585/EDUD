class ZCL_LGTICA_TRANSPORTE definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_TRANSPORTE
*"* do not include other source files here!!!
public section.

  class-methods CREAR_TRANSPORTE
    importing
      !I_PUESTO_TRAB type TPLST
      !I_CLASE_TRANS type SHTYP
      !I_TIPO_DOC type ZE_TIPO_DOC optional
      !I_USUARIO type ZED_USUARIO_MOVIL optional
    exporting
      !E_NUMERO_TRANS type TKNUM
      !E_NUMERO_TRANS_SE type ZED_TKNUM
    changing
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods ASOCIAR_DOCUMENTO
    importing
      !I_NUMERO_TRANS type TKNUM
      !I_NUMERO_TRANS_SE type ZED_TKNUM optional
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
      !I_ENTREGA type VBELN_VL optional
    changing
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods DESASOCIAR_ENTREGA
    importing
      !I_NUMERO_TRANS type TKNUM
      !I_ENTREGA type VBELN
    changing
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods INICIAR_DESPACHO
    importing
      !I_TRANSPORTE type TKNUM
    exporting
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods FINALIZAR_DESPACHO
    importing
      !I_TRANSPORTE type TKNUM
    exporting
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods PLANIFICAR_TRANSPORTE
    importing
      !I_TRANSPORTE type TKNUM
    exporting
      !C_RETURN type ZTTSD_MSG_PACKING .
  class-methods GET_DATA_BY_DOC
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
    exporting
      !E_TRANSPORTE type ZMM_TT_TRANSPORTE
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
protected section.
*"* protected components of class ZCL_LGTICA_TRANSPORTE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_TRANSPORTE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_TRANSPORTE IMPLEMENTATION.


METHOD ASOCIAR_DOCUMENTO.

*  Declaración de Variables
  DATA: lv_tknum                 TYPE tknum,
        lv_vbeln                 TYPE vbeln,
        lv_number(10)            TYPE p,
        lv_log_always            TYPE c,
        lv_log_on_error_warning  TYPE c,
        lv_save_log              TYPE c,
        lv_error                 TYPE c,
        lv_count                 TYPE i.

*  Declaración de Tablas internas
  DATA: lti_xvttp      TYPE TABLE OF  vttpvb,
        lti_yvttp      TYPE TABLE OF  vttpvb,
        lti_xvtsp      TYPE TABLE OF  vtspvb,
        lti_yvtsp      TYPE TABLE OF  vtspvb,
        lti_deliveries TYPE TABLE OF  ship_deliv,
        lti_xtrlk      TYPE TABLE OF  vtrlk,
        lti_xtrlp      TYPE TABLE OF  vtrlp,
        lti_xvtts      TYPE TABLE OF  vttsvb,
        lti_params     TYPE TABLE OF  rsparams,
        lti_yttkvb     TYPE TABLE OF vttkvb,
        lti_vttkvb     TYPE TABLE OF vttkvb.

*  Declaración de Estructuras
  DATA: les_vttkvb     TYPE vttkvb,
        les_xvttp      LIKE LINE OF   lti_xvttp,
        les_deliveries LIKE LINE OF   lti_deliveries,
        les_return     TYPE zesd_msg_packing.

*  Datos tablas Z.
  DATA: lti_transporte    TYPE TABLE OF zmm_t_transporte,
        les_transporte    TYPE zmm_t_transporte,
        lv_subrc          LIKE sy-subrc.

*  Si el tipo de documento es entrega se crea con estandar, de lo contrario
*  solo se guarda en tablas z.
  IF i_tipo_doc EQ 'ENTR'.
*   Validar que la entrega no este asociada a otro transporte.
    SELECT SINGLE tknum
      INTO lv_tknum
      FROM vttp WHERE vbeln = i_documento.
    IF sy-subrc EQ 0.
      les_return-num_doc = i_numero_trans.
*{   REPLACE        ER3K900279                                        7
*\      les_return-type_msg = 'E'.
*\      CONCATENATE 'Entrega:' i_documento 'asociada a transporte' lv_tknum
*\        INTO les_return-msg SEPARATED BY space.
      les_return-type_msg = 'E'.
      MESSAGE S004(ZCLPACK) WITH i_documento lv_tknum INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.
    CHECK c_return IS INITIAL.

*    Asignar el numero de entrega y transporte a variables locales.
    lv_tknum = i_numero_trans.

*..... Elimina ceros a la izquierda para que encuentre la cabecera de la entregas
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = i_documento
     IMPORTING
       OUTPUT        = lv_vbeln
              .

*..... Pone ceros a la izquierda para completar el documento
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = lv_vbeln
     IMPORTING
       OUTPUT        = lv_vbeln
              .

*
*    lv_vbeln = i_documento.

*    Obtener datos de cabecera de transporte.
    CALL FUNCTION 'RV_SHIPMENT_VIEW'
      EXPORTING
        shipment_number  = lv_tknum
        option_items     = 'X'
      IMPORTING
        f_vttkvb         = les_vttkvb
      TABLES
        f_vttp           = lti_xvttp
        f_trlk           = lti_xtrlk
        f_trlp           = lti_xtrlp
      EXCEPTIONS
        not_found        = 1
        no_authority     = 2
        delivery_missing = 3
        delivery_lock    = 4
        OTHERS           = 5.
    IF sy-subrc EQ 1.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                        8
*\      CONCATENATE 'Transporte' lv_tknum 'no existe.'
*\        INTO les_return-msg SEPARATED BY space.
      MESSAGE S005(ZCLPACK) WITH  lv_tknum INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ELSEIF sy-subrc NE 0.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                        9
*\      CONCATENATE 'Error obteniendo datos de cabecera' lv_tknum
*\        INTO les_return-msg SEPARATED BY space.
      MESSAGE S006(ZCLPACK) WITH  lv_tknum INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.

    CHECK c_return IS INITIAL.
*    Inicializar contador.
    lv_count = 1.

    LOOP AT lti_xvttp INTO les_xvttp.

      les_deliveries-vbeln = les_xvttp-vbeln.
      les_deliveries-tprfo = lv_count.
      APPEND les_deliveries TO lti_deliveries.
      lv_count = lv_count + 1.

      les_xvttp-updkz = 'U'.
      APPEND les_xvttp TO lti_yvttp.

    ENDLOOP.

    CLEAR les_xvttp.
    les_xvttp-vbeln = lv_vbeln.
    les_xvttp-updkz = 'I'.
    APPEND les_xvttp TO lti_xvttp.

    les_deliveries-vbeln = lv_vbeln.
    les_deliveries-tprfo = lv_count.
    APPEND les_deliveries TO lti_deliveries.

*    Obtener datos de transporte de la entrega
    CALL FUNCTION 'SD_SHIPMENT_DELIVERY_VIEW'
      EXPORTING
        i_langu                       = sy-langu
        i_partner_role                = ' '
        opt_minimized_delivery_view   = ' '
        opt_items                     = 'X'
        opt_status_data               = 'X'
        opt_partners                  = 'X'
        opt_customer_data             = 'X'
        opt_customizing_data          = 'X'
        opt_order_data                = ' '
        opt_export_data               = ' '
        opt_stawn_data                = ' '
        opt_hazmat_dat                = ' '
        i_filter_type                 = 'F'
      IMPORTING
        e_not_all_deliveries_found    = lv_error
      TABLES
        i_deliv                       = lti_deliveries
        c_vtrlk                       = lti_xtrlk
        c_vtrlp                       = lti_xtrlp
      EXCEPTIONS
        no_deliveries                 = 1
        error_reading_delivery_header = 2
        error_reading_partner_table   = 3
        OTHERS                        = 4.

    IF sy-subrc EQ 1.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       10
*\      les_return-msg = 'Entrega no existe'.
      MESSAGE S007(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ELSEIF sy-subrc EQ 2.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       11
*\      les_return-msg = 'Error leyendo cabecera de entrega'.
      MESSAGE S008(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ELSEIF sy-subrc EQ 3.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       12
*\      les_return-msg = 'Error leyendo datos de entrega'.
      MESSAGE S009(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.

    CHECK c_return IS INITIAL.
*    Asignar entrega al transporte.
    CALL FUNCTION 'SD_DELIVERY_ASSIGN_TO_SHIPMENT'
      EXPORTING
        i_tknum                = lv_tknum
      TABLES
        c_xvttp                = lti_xvttp
        c_yvttp                = lti_yvttp
        c_xvtsp                = lti_xvtsp
        c_yvtsp                = lti_yvtsp
        i_deliveries           = lti_deliveries
        i_xtrlk                = lti_xtrlk
        i_xtrlp                = lti_xtrlp
        i_xvtts                = lti_xvtts
      CHANGING
        c_xvttk                = les_vttkvb
      EXCEPTIONS
        no_transport_relevance = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       13
*\      les_return-msg = 'Error asignando entrega a transporte'.
      MESSAGE S010(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.
    CHECK c_return IS INITIAL.

    APPEND les_vttkvb TO lti_vttkvb.

*    Guardar datos de cabecera de transporte y sus posiciones.
    CALL FUNCTION 'SD_SHIPMENTS_SAVE'
      EXPORTING
        i_transaktionstyp      = 'V'
        i_upd_deadlines        = 'X'
        i_upd_texts            = 'X'
        i_upd_shipping_units   = 'X'
        i_upd_output           = 'X'
        i_opt_update_task      = 'X'
        i_opt_activities       = 'X'
        i_opt_no_statistics    = 'X'
        i_opt_read_ship_units  = 'X'
*{   REPLACE        ER3K900279                                        3
*\        i_flag_tra_complete    = 'C'
        i_flag_tra_complete    = 'A'
*}   REPLACE
        i_tra_save_caller      = 'DIA'
      IMPORTING
        e_log_always           = lv_log_always
        e_log_on_error_warning = lv_log_on_error_warning
        e_save_log             = lv_save_log
      TABLES
        i_xvttk                = lti_vttkvb
        i_yvttk                = lti_vttkvb
        i_xvttp                = lti_xvttp
        i_yvttp                = lti_yvttp
        i_vtrlk                = lti_xtrlk
        i_vtrlp                = lti_xtrlp
      EXCEPTIONS
        no_change              = 1
        delivery_split_error   = 2
        OTHERS                 = 3.

    IF sy-subrc EQ 0.
*{   INSERT         ER3K900279                                        1
       CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT          = 'X'
*        IMPORTING
*          RETURN        =
                 .

*}   INSERT
      COMMIT WORK AND WAIT.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'S'.
*{   REPLACE        ER3K900279                                       14
*\      les_return-msg = 'Transporte guardado correctamente'.
      MESSAGE S011(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.

*{   REPLACE        ER3K900279                                        5
*\      SELECT SINGLE *
*\        FROM zmm_t_transporte
*\        INTO les_transporte
*\        WHERE tknum = i_numero_trans.
      SELECT SINGLE MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
        FROM zmm_t_transporte
        INTO les_transporte
        WHERE tknum = i_numero_trans and
        tipo_doc  = I_TIPO_DOC.
*}   REPLACE

      les_transporte-tknum = i_numero_trans.
      les_transporte-tipo_doc = i_tipo_doc.
*{   REPLACE        ER3K900339                                        2
*\      les_transporte-documento = i_documento.
     les_transporte-tknum_se = i_numero_trans.
*..... Pone ceros a la izquierda para completar el documento
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = i_documento
     IMPORTING
       OUTPUT        = les_transporte-documento.
      les_transporte-vbeln = i_entrega.
*}   REPLACE
      les_transporte-fecha = sy-datum.
      les_transporte-hora  = sy-uzeit.
      les_transporte-uname = sy-uname.
      APPEND les_transporte TO lti_transporte.

*      Modificar datos de tablas de packing.
      CALL METHOD zcl_lgtica_util=>modify_zmm_t_transporte
        EXPORTING
          i_ti_transporte = lti_transporte
          i_mod           = 'M'
        IMPORTING
*          e_dbcnt      =
          e_subrc      = lv_subrc.

    ELSE.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       15
*\      les_return-msg = 'Error guardando el transporte'.
      MESSAGE S012(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.

  ELSE.
*{   REPLACE        ER3K900279                                        6
*\*    Codigo para tipo de dcto dfte a entrega.
*\          SELECT SINGLE *
*\        FROM zmm_t_transporte
*\        INTO les_transporte
*\        WHERE TKNUM_SE = i_numero_trans_se.
*\
*\      les_transporte-TKNUM_SE = i_numero_trans_se.
*\      les_transporte-tipo_doc = i_tipo_doc.
*\      les_transporte-documento = i_documento.
*\      les_transporte-fecha = sy-datum.
*\      les_transporte-hora  = sy-uzeit.
*\      les_transporte-uname = sy-uname.
*\      APPEND les_transporte TO lti_transporte.
*..... pone ceros a la izquierda para que encuentre la cabecera si está asociada
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = i_entrega
     IMPORTING
       OUTPUT        = lv_vbeln.

*   Validar que la entrega no este asociada a otro transporte.
    SELECT SINGLE tknum
      INTO lv_tknum
      FROM vttp WHERE vbeln = lv_vbeln.
    IF sy-subrc EQ 0.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      CONCATENATE 'Entrega:' i_documento 'asociada a transporte' lv_tknum
*        INTO les_return-msg SEPARATED BY space.
      MESSAGE S004(ZCLPACK) WITH i_documento lv_tknum INTO les_return-msg .
      APPEND les_return TO c_return.
    ENDIF.
    CHECK c_return IS INITIAL.

*    Asignar el numero de entrega y transporte a variables locales.
    lv_tknum = i_numero_trans.

*..... Elimina ceros a la izquierda para que encuentre la cabecera de la entregas
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = i_entrega
     IMPORTING
       OUTPUT        = lv_vbeln
              .

*..... Pone ceros a la izquierda para completar el documento
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = lv_vbeln
     IMPORTING
       OUTPUT        = lv_vbeln
              .

*
*    lv_vbeln = i_documento.

*    Obtener datos de cabecera de transporte.
    CALL FUNCTION 'RV_SHIPMENT_VIEW'
      EXPORTING
        shipment_number  = lv_tknum
        option_items     = 'X'
      IMPORTING
        f_vttkvb         = les_vttkvb
      TABLES
        f_vttp           = lti_xvttp
        f_trlk           = lti_xtrlk
        f_trlp           = lti_xtrlp
      EXCEPTIONS
        not_found        = 1
        no_authority     = 2
        delivery_missing = 3
        delivery_lock    = 4
        OTHERS           = 5.
    IF sy-subrc EQ 1.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      CONCATENATE 'Transporte' lv_tknum 'no existe.'
*        INTO les_return-msg SEPARATED BY space.
      MESSAGE S005(ZCLPACK) WITH lv_tknum INTO les_return-msg .
      APPEND les_return TO c_return.
    ELSEIF sy-subrc NE 0.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      CONCATENATE 'Error obteniendo datos de cabecera' lv_tknum
*        INTO les_return-msg SEPARATED BY space.
      MESSAGE S006(ZCLPACK) WITH lv_tknum INTO les_return-msg .
      APPEND les_return TO c_return.
    ENDIF.

    CHECK c_return IS INITIAL.
*    Inicializar contador.
    lv_count = 1.

    LOOP AT lti_xvttp INTO les_xvttp.

      les_deliveries-vbeln = les_xvttp-vbeln.
      les_deliveries-tprfo = lv_count.
      APPEND les_deliveries TO lti_deliveries.
      lv_count = lv_count + 1.

      les_xvttp-updkz = 'U'.
      APPEND les_xvttp TO lti_yvttp.

    ENDLOOP.

    CLEAR les_xvttp.
    les_xvttp-vbeln = lv_vbeln.
    les_xvttp-updkz = 'I'.
    APPEND les_xvttp TO lti_xvttp.

    les_deliveries-vbeln = lv_vbeln.
    les_deliveries-tprfo = lv_count.
    APPEND les_deliveries TO lti_deliveries.

*    Obtener datos de transporte de la entrega
    CALL FUNCTION 'SD_SHIPMENT_DELIVERY_VIEW'
      EXPORTING
        i_langu                       = sy-langu
        i_partner_role                = ' '
        opt_minimized_delivery_view   = ' '
        opt_items                     = 'X'
        opt_status_data               = 'X'
        opt_partners                  = 'X'
        opt_customer_data             = 'X'
        opt_customizing_data          = 'X'
        opt_order_data                = ' '
        opt_export_data               = ' '
        opt_stawn_data                = ' '
        opt_hazmat_dat                = ' '
        i_filter_type                 = 'F'
      IMPORTING
        e_not_all_deliveries_found    = lv_error
      TABLES
        i_deliv                       = lti_deliveries
        c_vtrlk                       = lti_xtrlk
        c_vtrlp                       = lti_xtrlp
      EXCEPTIONS
        no_deliveries                 = 1
        error_reading_delivery_header = 2
        error_reading_partner_table   = 3
        OTHERS                        = 4.

    IF sy-subrc EQ 1.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      les_return-msg = 'Entrega no existe'.
      MESSAGE S007(ZCLPACK) INTO les_return-msg .
      APPEND les_return TO c_return.
    ELSEIF sy-subrc EQ 2.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      les_return-msg = 'Error leyendo cabecera de entrega'.
      MESSAGE S008(ZCLPACK) INTO les_return-msg .
      APPEND les_return TO c_return.
    ELSEIF sy-subrc EQ 3.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      les_return-msg = 'Error leyendo datos de entrega'.
      MESSAGE S009(ZCLPACK) INTO les_return-msg .
      APPEND les_return TO c_return.
    ENDIF.

    CHECK c_return IS INITIAL.
*    Asignar entrega al transporte.
    CALL FUNCTION 'SD_DELIVERY_ASSIGN_TO_SHIPMENT'
      EXPORTING
        i_tknum                = lv_tknum
      TABLES
        c_xvttp                = lti_xvttp
        c_yvttp                = lti_yvttp
        c_xvtsp                = lti_xvtsp
        c_yvtsp                = lti_yvtsp
        i_deliveries           = lti_deliveries
        i_xtrlk                = lti_xtrlk
        i_xtrlp                = lti_xtrlp
        i_xvtts                = lti_xvtts
      CHANGING
        c_xvttk                = les_vttkvb
      EXCEPTIONS
        no_transport_relevance = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*      les_return-msg = 'Error asignando entrega a transporte'.
      MESSAGE S010(ZCLPACK) INTO les_return-msg .
      APPEND les_return TO c_return.
    ENDIF.
    CHECK c_return IS INITIAL.

    APPEND les_vttkvb TO lti_vttkvb.

*    Guardar datos de cabecera de transporte y sus posiciones.
    CALL FUNCTION 'SD_SHIPMENTS_SAVE'
      EXPORTING
        i_transaktionstyp      = 'V'
        i_upd_deadlines        = 'X'
        i_upd_texts            = 'X'
        i_upd_shipping_units   = 'X'
        i_upd_output           = 'X'
        i_opt_update_task      = 'X'
        i_opt_activities       = 'X'
        i_opt_no_statistics    = 'X'
        i_opt_read_ship_units  = 'X'
*{   REPLACE        ER3K900279                                        3
*\        i_flag_tra_complete    = 'C'
        i_flag_tra_complete    = 'A'
*}   REPLACE
        i_tra_save_caller      = 'DIA'
      IMPORTING
        e_log_always           = lv_log_always
        e_log_on_error_warning = lv_log_on_error_warning
        e_save_log             = lv_save_log
      TABLES
        i_xvttk                = lti_vttkvb
        i_yvttk                = lti_vttkvb
        i_xvttp                = lti_xvttp
        i_yvttp                = lti_yvttp
        i_vtrlk                = lti_xtrlk
        i_vtrlp                = lti_xtrlp
      EXCEPTIONS
        no_change              = 1
        delivery_split_error   = 2
        OTHERS                 = 3.

    IF sy-subrc EQ 0.
*{   INSERT         ER3K900279                                        1
       CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT          = 'X'
*        IMPORTING
*          RETURN        =
                 .

*}   INSERT
      COMMIT WORK AND WAIT.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'S'.
*{   REPLACE        ER3K900279                                       16
*\      les_return-msg = 'Transporte guardado correctamente'.
      MESSAGE S011(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.

*{   REPLACE        ER3K900279                                        5
*\      SELECT SINGLE *
*\        FROM zmm_t_transporte
*\        INTO les_transporte
*\        WHERE tknum = i_numero_trans.
      SELECT SINGLE MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
        FROM zmm_t_transporte
        INTO les_transporte
        WHERE tknum = i_numero_trans and
        tipo_doc  = I_TIPO_DOC.
*}   REPLACE

      les_transporte-tknum = i_numero_trans.
      les_transporte-tipo_doc = i_tipo_doc.
      les_transporte-vbeln = i_entrega.
*{   REPLACE        ER3K900339                                        2
*\      les_transporte-documento = i_documento.
     les_transporte-tknum_se = i_numero_trans.
*..... Pone ceros a la izquierda para completar el documento
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = i_documento
     IMPORTING
       OUTPUT        = les_transporte-documento.

*}   REPLACE
      les_transporte-fecha = sy-datum.
      les_transporte-hora  = sy-uzeit.
      les_transporte-uname = sy-uname.
      APPEND les_transporte TO lti_transporte.

*      Modificar datos de tablas de packing.
      CALL METHOD zcl_lgtica_util=>modify_zmm_t_transporte
        EXPORTING
          i_ti_transporte = lti_transporte
          i_mod           = 'M'
        IMPORTING
*          e_dbcnt      =
          e_subrc      = lv_subrc.

    ELSE.
      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'E'.
*{   REPLACE        ER3K900279                                       17
*\      les_return-msg = 'Error guardando el transporte'.
      MESSAGE S012(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.
    ENDIF.

*}   REPLACE

*      Modificar datos de tablas de packing.
      CALL METHOD zcl_lgtica_util=>modify_zmm_t_transporte
        EXPORTING
          i_ti_transporte = lti_transporte
          i_mod           = 'M'
        IMPORTING
*          e_dbcnt      =
          e_subrc      = lv_subrc.

      les_return-num_doc = i_numero_trans.
      les_return-type_msg = 'S'.
*{   REPLACE        ER3K900279                                       18
*\      les_return-msg = 'Transporte guardado correctamente'.
      MESSAGE S011(ZCLPACK) INTO les_return-msg .
*}   REPLACE
      APPEND les_return TO c_return.


  ENDIF.

ENDMETHOD.


METHOD crear_transporte.

*  Declaración de variables.
  DATA: les_headerdata TYPE bapishipmentheader,
        lti_return     TYPE TABLE OF bapiret2,
        les_return     LIKE LINE OF lti_return,
        les_creturn    TYPE zesd_msg_packing.

  DATA: lti_transporte    TYPE TABLE OF zmm_t_transporte,
        les_transporte    TYPE zmm_t_transporte,
        lv_subrc          LIKE sy-subrc.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDTRANSPO', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado


*{   DELETE         ER3K900346                                        3
*\  IF i_tipo_doc EQ 'ENTR'.
*}   DELETE
*   Mapeo de datos de cabecera.
    les_headerdata-trans_plan_pt = i_puesto_trab.
    les_headerdata-shipment_type = i_clase_trans.
*   change Plan status
*    les_headerdata-status_plan = 'X'.

*    Creación de transporte.
    CALL FUNCTION 'BAPI_SHIPMENT_CREATE'
      EXPORTING
        headerdata = les_headerdata
      IMPORTING
        transport  = e_numero_trans
      TABLES
        return     = lti_return.

*    Recorrer mensaje de retorno.
    LOOP AT lti_return INTO les_return.
      les_creturn-num_doc = les_return-message_v1.
      les_creturn-msg = les_return-message.
      les_creturn-type_msg = les_return-type.
      APPEND les_creturn TO c_return.
    ENDLOOP.

    CHECK e_numero_trans IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*    les_transporte-tknum = e_numero_trans.
*    les_transporte-fecha = sy-datum.
*    les_transporte-hora  = sy-uzeit.
*    les_transporte-uname = sy-uname.
*    les_transporte-usuario = i_usuario.
*    APPEND les_transporte TO lti_transporte.
*
**    Guardar datos en tablas Z de packing.
*    CALL METHOD zcl_lgtica_util=>modify_zmm_t_transporte
*      EXPORTING
*        i_ti_transporte = lti_transporte
*        i_mod           = 'I'
*      IMPORTING
**        e_dbcnt      =
*        e_subrc      = lv_subrc.
*{   DELETE         ER3K900346                                        1
*\  ELSE.
*\*    Crear transporte solo guardando en tablas z.
*\            CALL FUNCTION 'NUMBER_GET_NEXT'
*\          EXPORTING
*\            nr_range_nr             = wnorange
*\            object                  = wobjeto
*\            subobject               = wsubobj
*\          IMPORTING
*\            number                  = p_doc_number " Número generado por SAP
*\          EXCEPTIONS
*\            interval_not_found      = 1
*\            number_range_not_intern = 2
*\            object_not_found        = 3
*\            quantity_is_0           = 4
*\            quantity_is_not_1       = 5
*\            internal_overflow       = 6
*\            OTHERS                  = 7.
*\
*\            E_NUMERO_TRANS_SE = p_doc_number.
*}   DELETE

*{   DELETE         ER3K900346                                        2
*\  ENDIF.
*}   DELETE

ENDMETHOD.


METHOD desasociar_entrega.

*  Declaración de Variables
  DATA: lv_tknum                 TYPE tknum,
        lv_vbeln                 TYPE vbeln,
        lv_number(10)            TYPE p,
        lv_log_always            TYPE c,
        lv_log_on_error_warning  TYPE c,
        lv_save_log              TYPE c,
        lv_error                 TYPE c,
        lv_count                 TYPE i.

*  Declaración de Tablas internas
  DATA: lti_xvttp      TYPE TABLE OF  vttpvb,
        lti_yvttp      TYPE TABLE OF  vttpvb,
        lti_xvtsp      TYPE TABLE OF  vtspvb,
        lti_yvtsp      TYPE TABLE OF  vtspvb,
        lti_deliveries TYPE TABLE OF  ship_deliv,
        lti_xtrlk      TYPE TABLE OF  vtrlk,
        lti_xtrlp      TYPE TABLE OF  vtrlp,
        lti_xvtts      TYPE TABLE OF  vttsvb,
        lti_params     TYPE TABLE OF  rsparams,
        lti_yttkvb     TYPE TABLE OF vttkvb,
        lti_vttkvb     TYPE TABLE OF vttkvb.

*  Declaración de Estructuras
  DATA: les_vttkvb     TYPE vttkvb,
        les_xvttp      LIKE LINE OF   lti_xvttp,
        les_deliveries LIKE LINE OF   lti_deliveries,
        les_return     TYPE zesd_msg_packing.

*  Datos tablas Z.
  DATA: lti_transporte    TYPE TABLE OF zmm_t_transporte,
        les_transporte    TYPE zmm_t_transporte,
        lv_subrc       LIKE sy-subrc.

*  Asignar el numero de entrega y transporte a variables locales.
  lv_tknum = i_numero_trans.
  lv_vbeln = i_entrega.

*  Obtener datos de cabecera de transporte.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number  = lv_tknum
      option_items     = 'X'
    IMPORTING
      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
    EXCEPTIONS
      not_found        = 1
      no_authority     = 2
      delivery_missing = 3
      delivery_lock    = 4
      OTHERS           = 5.
  IF sy-subrc EQ 1.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Transporte no existe'.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK sy-subrc EQ 0.
*  Inicializar contador.
  lv_count = 1.
*  Validar que entrega este asociada al transporte.
  READ TABLE lti_xvttp INTO les_xvttp
    WITH KEY vbeln = lv_vbeln.
  IF sy-subrc NE 0.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Entrega no esta asociada al Transporte'.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK sy-subrc EQ 0.

*  Recorremos la tabal de posiciones y modificamos registros
*  que se desean eliminar.
  LOOP AT lti_xvttp INTO les_xvttp.

    les_deliveries-vbeln = les_xvttp-vbeln.
    les_deliveries-tprfo = lv_count.
    APPEND les_deliveries TO lti_deliveries.
    lv_count = lv_count + 1.

    IF les_xvttp-vbeln EQ lv_vbeln.
      les_xvttp-updkz = 'D'.
    ELSE.
      les_xvttp-updkz = 'U'.
    ENDIF.

    APPEND les_xvttp TO lti_yvttp.

  ENDLOOP.

*  Obtener datos de transporte de la entrega
  CALL FUNCTION 'SD_SHIPMENT_DELIVERY_VIEW'
    EXPORTING
      i_langu                       = sy-langu
      i_partner_role                = ' '
      opt_minimized_delivery_view   = ' '
      opt_items                     = 'X'
      opt_status_data               = 'X'
      opt_partners                  = 'X'
      opt_customer_data             = 'X'
      opt_customizing_data          = 'X'
      opt_order_data                = ' '
      opt_export_data               = ' '
      opt_stawn_data                = ' '
      opt_hazmat_dat                = ' '
      i_filter_type                 = 'F'
    IMPORTING
      e_not_all_deliveries_found    = lv_error
    TABLES
      i_deliv                       = lti_deliveries
      c_vtrlk                       = lti_xtrlk
      c_vtrlp                       = lti_xtrlp
    EXCEPTIONS
      no_deliveries                 = 1
      error_reading_delivery_header = 2
      error_reading_partner_table   = 3
      OTHERS                        = 4.

  IF sy-subrc EQ 1.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Entrega no existe'.
    APPEND les_return TO c_return.
  ELSEIF sy-subrc EQ 2.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error leyendo cabecera de entrega'.
    APPEND les_return TO c_return.
  ELSEIF sy-subrc EQ 3.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error leyendo datos de entrega'.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK sy-subrc EQ 0.
*  Asignar entrega al transporte.
  CALL FUNCTION 'SD_DELIVERY_ASSIGN_TO_SHIPMENT'
    EXPORTING
      i_tknum                = lv_tknum
    TABLES
      c_xvttp                = lti_xvttp
      c_yvttp                = lti_yvttp
      c_xvtsp                = lti_xvtsp
      c_yvtsp                = lti_yvtsp
      i_deliveries           = lti_deliveries
      i_xtrlk                = lti_xtrlk
      i_xtrlp                = lti_xtrlp
      i_xvtts                = lti_xvtts
    CHANGING
      c_xvttk                = les_vttkvb
    EXCEPTIONS
      no_transport_relevance = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error asignado entrega a transporte'.
    APPEND les_return TO c_return.
  ENDIF.
  CHECK sy-subrc EQ 0.

  APPEND les_vttkvb TO lti_vttkvb.

*  Guardar datos de cabecera de transporte y sus posiciones.
  CALL FUNCTION 'SD_SHIPMENTS_SAVE'
    EXPORTING
      i_transaktionstyp      = 'V'
      i_upd_deadlines        = 'X'
      i_upd_texts            = 'X'
      i_upd_shipping_units   = 'X'
      i_upd_output           = 'X'
      i_opt_update_task      = 'X'
      i_opt_activities       = 'X'
      i_opt_no_statistics    = 'X'
      i_opt_read_ship_units  = 'X'
      i_flag_tra_complete    = 'C'
      i_tra_save_caller      = 'DIA'
    IMPORTING
      e_log_always           = lv_log_always
      e_log_on_error_warning = lv_log_on_error_warning
      e_save_log             = lv_save_log
    TABLES
      i_xvttk                = lti_vttkvb
      i_yvttk                = lti_vttkvb
      i_xvttp                = lti_xvttp
      i_yvttp                = lti_yvttp
      i_vtrlk                = lti_xtrlk
      i_vtrlp                = lti_xtrlp
    EXCEPTIONS
      no_change              = 1
      delivery_split_error   = 2
      OTHERS                 = 3.

  IF sy-subrc EQ 0.

    COMMIT WORK.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'S'.
    les_return-msg = 'Transporte guardado correctamente'.
    APPEND les_return TO c_return.

*    Mapear datos para tablas z.
    les_transporte-tknum = i_numero_trans.
    les_transporte-tipo_doc = ''.
    les_transporte-documento = ''.
    APPEND les_transporte TO lti_transporte.

*    Modificar datos de tablas de packing.
    CALL METHOD zcl_lgtica_util=>MODIFY_ZMM_T_TRANSPORTE
      EXPORTING
        i_ti_transporte = lti_transporte
        i_mod           = 'U'
      IMPORTING
*        e_dbcnt      =
        e_subrc      = lv_subrc.
  ELSE.
    les_return-num_doc = i_numero_trans.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error guardando el transporte'.
    APPEND les_return TO c_return.
  ENDIF.

ENDMETHOD.


METHOD FINALIZAR_DESPACHO.

*  Declaración de Variables
  DATA: lv_tknum                 TYPE tknum,
        lv_vbeln                 TYPE vbeln,
        lv_number(10)            TYPE p,
        lv_log_always            TYPE c,
        lv_log_on_error_warning  TYPE c,
        lv_save_log              TYPE c,
        lv_error                 TYPE c,
        lv_count                 TYPE i.

*  Declaración de Tablas internas
  DATA: lti_xvttp      TYPE TABLE OF  vttpvb,
        lti_yvttp      TYPE TABLE OF  vttpvb,
        lti_xvtsp      TYPE TABLE OF  vtspvb,
        lti_yvtsp      TYPE TABLE OF  vtspvb,
        lti_deliveries TYPE TABLE OF  ship_deliv,
        lti_xtrlk      TYPE TABLE OF  vtrlk,
        lti_xtrlp      TYPE TABLE OF  vtrlp,
        lti_xvtts      TYPE TABLE OF  vttsvb,
        lti_params     TYPE TABLE OF  rsparams,
        lti_yttkvb     TYPE TABLE OF  vttkvb,
        lti_vttkvb     TYPE TABLE OF  vttkvb.

*  Declaración de Estructuras
  DATA: les_vttkvb     TYPE vttkvb,
        les_xvttp      LIKE LINE OF   lti_xvttp,
        les_return     TYPE zesd_msg_packing.

  DATA: lti_vttsvb  TYPE TABLE OF vttsvb,
        lti_vttsvb2 TYPE TABLE OF vttsvb,
        lti_vtspvb  TYPE TABLE OF vtspvb,
        lti_vtspvb2 TYPE TABLE OF vtspvb,
        lti_vbpavb  TYPE TABLE OF vbpavb,
        lti_vbpavb2 TYPE TABLE OF vbpavb,
        lti_sadrvb  TYPE TABLE OF sadrvb,
        lti_sadrvb2 TYPE TABLE OF sadrvb.


  lv_tknum = i_transporte.

*    Obtener datos de cabecera de transporte.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number  = lv_tknum
      option_items     = 'X'
    IMPORTING
      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
    EXCEPTIONS
      not_found        = 1
      no_authority     = 2
      delivery_missing = 3
      delivery_lock    = 4
      OTHERS           = 5.
  IF sy-subrc EQ 1.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Transporte' lv_tknum 'no existe.'
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ELSEIF sy-subrc NE 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Error obteniendo datos de cabecera' lv_tknum
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK c_return IS INITIAL.

*  Mapeo de datos.
  APPEND les_vttkvb TO lti_vttkvb.
*{   DELETE         ER3K900332                                        7
*\  les_vttkvb-stabf = 'X'.
*\  les_vttkvb-dtabf = sy-datum.
*\  les_vttkvb-uzabf = sy-uzeit.
*}   DELETE
*{   INSERT         ER3K900332                                        8
  les_vttkvb-stlad = 'X'.
  les_vttkvb-dalen = sy-datum.
  les_vttkvb-ualen = sy-uzeit.
  LES_VTTKVB-PKSTK = 'X'.
*}   INSERT

*  Modificación de cabecera de transporte.
  CALL FUNCTION 'SD_SHIPMENT_HEADER_CHANGE'
    TABLES
      c_xvttk                            = lti_vttkvb
      c_yvttk                            = lti_yttkvb
      c_xvttp                            = lti_xvttp
      c_yvttp                            = lti_yvttp
      c_xvtts                            = lti_vttsvb
      c_yvtts                            = lti_vttsvb2
      c_xvtsp                            = lti_vtspvb
      c_yvtsp                            = lti_vtspvb2
      c_xvbpa                            = lti_vbpavb
      c_yvbpa                            = lti_vbpavb2
      c_xvbadr                           = lti_sadrvb
      c_yvbadr                           = lti_sadrvb
      i_xtrlk                            = lti_xtrlk
      i_xtrlp                            = lti_xtrlp
    CHANGING
      c_xvttk_new                        = les_vttkvb
   EXCEPTIONS
     invalid_change                     = 1
     route_insert_failed                = 2
     tdlnr_insert_failed                = 3
     status_planned_failed              = 4
     status_registrated_failed          = 5
     status_loading_start_failed        = 6
     status_loading_end_failed          = 7
     status_completion_failed           = 8
     status_shipment_start_failed       = 9
     status_shipment_end_failed         = 10
     OTHERS                             = 11
            .
  IF sy-subrc <> 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error modificando transporte'.
    APPEND les_return TO c_return.
  ENDIF.

  REFRESH lti_vttkvb[].
*{   DELETE         ER3K900332                                        9
*\  LES_VTTKVB-PKSTK = 'X'.
*}   DELETE
  APPEND les_vttkvb TO lti_vttkvb.

*    Guardar datos de cabecera de transporte y sus posiciones.
  CALL FUNCTION 'SD_SHIPMENTS_SAVE'
    EXPORTING
      i_transaktionstyp      = 'V'
      i_opt_update_task      = 'X'
    IMPORTING
      e_log_always           = lv_log_always
      e_log_on_error_warning = lv_log_on_error_warning
      e_save_log             = lv_save_log
    TABLES
      i_xvttk                = lti_vttkvb
      i_yvttk                = lti_yttkvb
      i_xvttp                = lti_xvttp
      i_yvttp                = lti_yvttp
      i_vtrlk                = lti_xtrlk
      i_vtrlp                = lti_xtrlp
    EXCEPTIONS
      no_change              = 1
      delivery_split_error   = 2
      OTHERS                 = 3.
*{   REPLACE        ER3K900332                                        5
*\
*\  IF sy-subrc EQ 0.

  IF sy-subrc EQ 0 or sy-subrc EQ 1.
*}   REPLACE
*{   REPLACE        ER3K900332                                        3
*\    COMMIT WORK.
    COMMIT WORK and WAIT    .
*}   REPLACE
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'S'.
    les_return-msg = 'Transporte guardado correctamente'.
    APPEND les_return TO c_return.
*{   INSERT         ER3K900332                                        6
**   Tabla para el Batch Input
*DATA: g_it_bdcdata type STANDARD TABLE OF bdcdata,
*      g_les_bdcdata type bdcdata,
*      messtab type STANDARD TABLE OF bdcmsgcoll,
*      les_messtab type bdcmsgcoll.
*
*
*DATA: cb_9001_flag TYPE c.
*DATA: g_wa_vuelta TYPE i VALUE 1.
*
*
*
*  CLEAR g_it_bdcdata.
*
** Dynpro 9000
*  g_les_bdcdata-program = 'SAPMV56A'.
*  g_les_bdcdata-dynpro = 1011.
*  g_les_bdcdata-dynbegin = 'X'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'BDC_CURSOR'.
*  g_les_bdcdata-fval = '=VTTK-TKNUM'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'BDC_OKCODE'.
*  g_les_bdcdata-fval = '=MM_VSEB'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'VTTK-TKNUM'.
*  g_les_bdcdata-fval = i_transporte. " poner transporte
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
** Dynpro 9002
*  clear g_les_bdcdata.
*  g_les_bdcdata-program = 'SAPLV51G'.
*  g_les_bdcdata-dynpro = 6000.
*  g_les_bdcdata-dynbegin = 'X'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'BDC_OKCODE'.
*  g_les_bdcdata-fval = '=SICH'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'BDC_SUBSCR'.
*  g_les_bdcdata-fval = 'SAPLV51G                                6010TAB'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
*  clear g_les_bdcdata.
*  g_les_bdcdata-fnam = 'BDC_CURSOR'.
*  g_les_bdcdata-fval = 'V51VE-VHILM(01)'.
*  APPEND g_les_bdcdata to g_it_bdcdata.
*
**     Ejecutamos el programa con la tabla BDCDATA.
*      CALL TRANSACTION 'VT02N' USING g_it_bdcdata mode 'N' MESSAGES INTO messtab .
*
*      READ TABLE messtab TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.
*
*IF sy-subrc NE 0.
*
*CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*EXPORTING
*wait = 'X'.
*
*ELSE.
*CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
**LOOP AT messtab into les_messtab.
*  les_return-num_doc = i_transporte.
*    les_return-type_msg = 'E'.
*    les_return-msg = 'Error sincronizando flujo de documentos '.
*    APPEND les_return TO c_return.
*ENDIF.
*ENDLOOP.
*}   INSERT

  ELSE.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error guardando el transporte'.
    APPEND les_return TO c_return.
  ENDIF.

ENDMETHOD.


method GET_DATA_BY_DOC.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Modulo de Función para Consultas de Cabeceras Documentos transporte
* Autor Prog.  :
* Fecha Creac. : 20.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para el Número de documento
DATA:  lv_documento TYPE ze_documento.
*  Estructura para el manejo de mensajes.
  DATA: les_mensajes TYPE zesd_msg_packing,
        lv_vbeln     TYPE vbeln.

    lv_documento = i_documento.

*.... Consulto transporte del documento
*{   REPLACE        ER3K900346                                        2
*\    SELECT *
    SELECT MANDT TKNUM TKNUM_SE  DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
*}   REPLACE
      INTO TABLE e_transporte
      FROM zmm_t_transporte
      WHERE documento  = lv_documento
      and tipo_doc = i_tipo_doc.
*{   REPLACE        ER3K900346                                        1
*\*  En caso de que no se encuentre el transporte se retorna mensaje de error.
*\  IF sy-subrc NE 0.
*\    les_mensajes-num_doc = lv_documento.
*\    CONCATENATE 'No existen datos de cabecera para documento' lv_documento
*\      INTO les_mensajes-msg SEPARATED BY space.
*\    les_mensajes-type_msg = 'E'.
*\    APPEND les_mensajes TO c_mensajes.
*\  ENDIF.
*  En caso de que no se encuentre el transporte se retorna mensaje de error.
  IF sy-subrc NE 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = lv_documento
     IMPORTING
       OUTPUT        = lv_documento
              .
*.... Consulto transporte del documento
    SELECT MANDT TKNUM TKNUM_SE  DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      INTO TABLE e_transporte
      FROM zmm_t_transporte
      WHERE documento  = lv_documento
      and tipo_doc = i_tipo_doc.
      IF sy-subrc NE 0.
    les_mensajes-num_doc = lv_documento.
*    CONCATENATE 'No existen datos de cabecera para documento' lv_documento
*      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S013(ZCLPACK) WITH lv_documento into les_mensajes-msg.
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
    ENDIF.
  ENDIF.
*}   REPLACE

endmethod.


METHOD INICIAR_DESPACHO.
*{   INSERT         ER3K900332                                        2

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing,
        lti_VSEP_T_RSEROB type VSEP_T_RSEROB,
        lti_HUM_HISTORY_T type HUM_HISTORY_T,
        lti_HUM_VENUM_T type HUM_VENUM_T,
        lti_HUITEM_MESSAGES_T type HUITEM_MESSAGES_T,
        les_OBJECTS TYPE  HUM_OBJECT.

*}   INSERT

*  Declaración de Variables
  DATA: lv_tknum                 TYPE tknum,
        lv_vbeln                 TYPE vbeln,
        lv_number(10)            TYPE p,
        lv_log_always            TYPE c,
        lv_log_on_error_warning  TYPE c,
        lv_save_log              TYPE c,
        lv_error                 TYPE c,
        lv_count                 TYPE i.

*  Declaración de Tablas internas
  DATA: lti_xvttp      TYPE TABLE OF  vttpvb,
*{   INSERT         ER3K900332                                        5
        lti_xvtfa type table of VTFAVB,
        les_xvtfa type VTFAVB,
*}   INSERT
        lti_yvttp      TYPE TABLE OF  vttpvb,
        lti_xvtsp      TYPE TABLE OF  vtspvb,
        lti_yvtsp      TYPE TABLE OF  vtspvb,
        lti_deliveries TYPE TABLE OF  ship_deliv,
        lti_xtrlk      TYPE TABLE OF  vtrlk,
        lti_xtrlp      TYPE TABLE OF  vtrlp,
        lti_xvtts      TYPE TABLE OF  vttsvb,
        lti_params     TYPE TABLE OF  rsparams,
        lti_yttkvb     TYPE TABLE OF  vttkvb,
        lti_vttkvb     TYPE TABLE OF  vttkvb.

*  Declaración de Estructuras
  DATA: les_vttkvb     TYPE vttkvb,
        les_xvttp      LIKE LINE OF   lti_xvttp,
        les_return     TYPE zesd_msg_packing.

  DATA: lti_vttsvb  TYPE TABLE OF vttsvb,
        lti_vttsvb2 TYPE TABLE OF vttsvb,
        lti_vtspvb  TYPE TABLE OF vtspvb,
        lti_vtspvb2 TYPE TABLE OF vtspvb,
        lti_vbpavb  TYPE TABLE OF vbpavb,
        lti_vbpavb2 TYPE TABLE OF vbpavb,
        lti_sadrvb  TYPE TABLE OF sadrvb,
        lti_sadrvb2 TYPE TABLE OF sadrvb.


  lv_tknum = i_transporte.
*{   INSERT         ER3K900332                                        3

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  les_OBJECTS-object = '04'.
  les_OBJECTS-objkey = lv_tknum.


     CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      IS_OBJECTS  = les_OBJECTS

    IMPORTING
      et_header   = lti_header
      et_items    = lti_huitem
      ET_ITEM_SERIALNO = lti_VSEP_T_RSEROB
      ET_HISTORY  = lti_HUM_HISTORY_T
      ET_HIGHEST_LEVELS = lti_HUM_VENUM_T
      ET_MESSAGES = lti_HUITEM_MESSAGES_T
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.


*}   INSERT

*    Obtener datos de cabecera de transporte.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number  = lv_tknum
      option_items     = 'X'
*{   INSERT         ER3K900332                                        9
       OPTION_PACKAGE_DIALOG = 'X'
*}   INSERT
    IMPORTING
      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
*{   INSERT         ER3K900332                                        8
      f_vtfa           = lti_xvtfa
*}   INSERT
    EXCEPTIONS
      not_found        = 1
      no_authority     = 2
      delivery_missing = 3
      delivery_lock    = 4
      OTHERS           = 5.
  IF sy-subrc EQ 1.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Transporte' lv_tknum 'no existe.'
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ELSEIF sy-subrc NE 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Error obteniendo datos de cabecera' lv_tknum
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK c_return IS INITIAL.

*  Mapeo de datos.
  APPEND les_vttkvb TO lti_vttkvb.
  les_vttkvb-stlad = 'X'.
  les_vttkvb-dalen = sy-datum.
  les_vttkvb-ualen = sy-uzeit.
*{   INSERT         ER3K900332                                        6
*SELECT MANDT TKNUM VBELV POSNV  VBTYP_V VBELN POSNN VBTYP_N RFMNG MEINS ERDAT ERZET AEDAT RFMNG_FLO RFMNG_FLT VRKME
*  into table lti_xvtfa
*  from  vtfa
*  where tknum eq lv_tknum.
*
*  LOOP AT lti_xvtfa into les_xvtfa .
*    les_xvtfa-updkz = 'I'.
*    MODIFY lti_xvtfa from les_xvtfa INDEX sy-tabix.
*  ENDLOOP.


*}   INSERT

*  Modificación de cabecera de transporte.
  CALL FUNCTION 'SD_SHIPMENT_HEADER_CHANGE'
*{   INSERT         ER3K900332                                       10
        EXPORTING
     OPT_DIALOG = ' '
*}   INSERT
    TABLES
      c_xvttk                            = lti_vttkvb
      c_yvttk                            = lti_yttkvb
      c_xvttp                            = lti_xvttp
      c_yvttp                            = lti_yvttp
      c_xvtts                            = lti_vttsvb
      c_yvtts                            = lti_vttsvb2
      c_xvtsp                            = lti_vtspvb
      c_yvtsp                            = lti_vtspvb2
      c_xvbpa                            = lti_vbpavb
      c_yvbpa                            = lti_vbpavb2
      c_xvbadr                           = lti_sadrvb
      c_yvbadr                           = lti_sadrvb
      i_xtrlk                            = lti_xtrlk
      i_xtrlp                            = lti_xtrlp
*{   INSERT         ER3K900332                                        7
      I_XVTFA                           = lti_xvtfa
*}   INSERT
    CHANGING
      c_xvttk_new                        = les_vttkvb
   EXCEPTIONS
     invalid_change                     = 1
     route_insert_failed                = 2
     tdlnr_insert_failed                = 3
     status_planned_failed              = 4
     status_registrated_failed          = 5
     status_loading_start_failed        = 6
     status_loading_end_failed          = 7
     status_completion_failed           = 8
     status_shipment_start_failed       = 9
     status_shipment_end_failed         = 10
     OTHERS                             = 11
            .
  IF sy-subrc <> 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error modificando transporte'.
    APPEND les_return TO c_return.
  ENDIF.

  REFRESH lti_vttkvb[].
  APPEND les_vttkvb TO lti_vttkvb.

*    Guardar datos de cabecera de transporte y sus posiciones.
*{   REPLACE        ER3K900332                                        4
*\  CALL FUNCTION 'SD_SHIPMENTS_SAVE'
*\    EXPORTING
*\      i_transaktionstyp      = 'V'
*\      i_opt_update_task      = 'X'
*\    IMPORTING
*\      e_log_always           = lv_log_always
*\      e_log_on_error_warning = lv_log_on_error_warning
*\      e_save_log             = lv_save_log
*\    TABLES
*\      i_xvttk                = lti_vttkvb
*\      i_yvttk                = lti_yttkvb
*\      i_xvttp                = lti_xvttp
*\      i_yvttp                = lti_yvttp
*\      i_vtrlk                = lti_xtrlk
*\      i_vtrlp                = lti_xtrlp
*\    EXCEPTIONS
*\      no_change              = 1
*\      delivery_split_error   = 2
*\      OTHERS                 = 3.

*CLEAR: lti_xvtfa.

*SELECT MANDT TKNUM VBELV POSNV  VBTYP_V VBELN POSNN VBTYP_N RFMNG MEINS ERDAT ERZET AEDAT RFMNG_FLO RFMNG_FLT VRKME
*  into table lti_xvtfa
*  from  vtfa
*  where tknum eq lv_tknum.
*
*  LOOP AT lti_xvtfa into les_xvtfa .
*    les_xvtfa-updkz = 'I'.
*    MODIFY lti_xvtfa from les_xvtfa INDEX sy-tabix.
*  ENDLOOP.

  CALL FUNCTION 'SD_SHIPMENTS_SAVE'
    EXPORTING
*      i_transaktionstyp      = 'V'
*      i_opt_update_task      = 'X'
*      I_UPD_SHIPPING_UNITS   = 'X'
      i_transaktionstyp      = 'V'
      i_upd_deadlines        = 'X'
      i_upd_texts            = 'X'
      i_upd_shipping_units   = 'X'
      i_upd_output           = 'X'
*      i_opt_update_task      = 'X'
*      i_opt_activities       = 'X'
*      i_opt_read_ship_units  = 'X'
*      i_flag_tra_complete    = 'C'
*      i_tra_save_caller      = 'DIA'
    IMPORTING
      e_log_always           = lv_log_always
      e_log_on_error_warning = lv_log_on_error_warning
      e_save_log             = lv_save_log
    TABLES
      i_xvttk                = lti_vttkvb
      i_yvttk                = lti_yttkvb
      i_xvttp                = lti_xvttp
      i_yvttp                = lti_yvttp
      i_vtrlk                = lti_xtrlk
      i_vtrlp                = lti_xtrlp
      i_xvtfa                = lti_xvtfa
      i_xvekp                = lti_header
    EXCEPTIONS
      no_change              = 1
      delivery_split_error   = 2
      OTHERS                 = 3.
*}   REPLACE
*{   REPLACE        ER3K900332                                        1
*\
*\  IF sy-subrc EQ 0.
*\    COMMIT WORK.

  IF sy-subrc EQ 0 or sy-subrc EQ 1.
    CALL FUNCTION 'HU_POST'
    EXPORTING
      IF_SYNCHRON = 'X'
      if_commit = 'X'.

    COMMIT WORK AND WAIT.
*}   REPLACE
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'S'.
    les_return-msg = 'Transporte guardado correctamente'.
    APPEND les_return TO c_return.

  ELSE.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error guardando el transporte'.
    APPEND les_return TO c_return.
  ENDIF.

ENDMETHOD.


METHOD PLANIFICAR_TRANSPORTE.

*  Declaración de Variables
  DATA: lv_tknum                 TYPE tknum,
        lv_vbeln                 TYPE vbeln,
        lv_number(10)            TYPE p,
        lv_log_always            TYPE c,
        lv_log_on_error_warning  TYPE c,
        lv_save_log              TYPE c,
        lv_error                 TYPE c,
        lv_count                 TYPE i.

*  Declaración de Tablas internas
  DATA: lti_xvttp      TYPE TABLE OF  vttpvb,
        lti_yvttp      TYPE TABLE OF  vttpvb,
        lti_xvtsp      TYPE TABLE OF  vtspvb,
        lti_yvtsp      TYPE TABLE OF  vtspvb,
        lti_deliveries TYPE TABLE OF  ship_deliv,
        lti_xtrlk      TYPE TABLE OF  vtrlk,
        lti_xtrlp      TYPE TABLE OF  vtrlp,
        lti_xvtts      TYPE TABLE OF  vttsvb,
        lti_params     TYPE TABLE OF  rsparams,
        lti_yttkvb     TYPE TABLE OF  vttkvb,
        lti_vttkvb     TYPE TABLE OF  vttkvb.

*  Declaración de Estructuras
  DATA: les_vttkvb     TYPE vttkvb,
        les_xvttp      LIKE LINE OF   lti_xvttp,
        les_return     TYPE zesd_msg_packing.

  DATA: lti_vttsvb  TYPE TABLE OF vttsvb,
        lti_vttsvb2 TYPE TABLE OF vttsvb,
        lti_vtspvb  TYPE TABLE OF vtspvb,
        lti_vtspvb2 TYPE TABLE OF vtspvb,
        lti_vbpavb  TYPE TABLE OF vbpavb,
        lti_vbpavb2 TYPE TABLE OF vbpavb,
        lti_sadrvb  TYPE TABLE OF sadrvb,
        lti_sadrvb2 TYPE TABLE OF sadrvb.


  lv_tknum = i_transporte.

*    Obtener datos de cabecera de transporte.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number  = lv_tknum
      option_items     = 'X'
    IMPORTING
      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
    EXCEPTIONS
      not_found        = 1
      no_authority     = 2
      delivery_missing = 3
      delivery_lock    = 4
      OTHERS           = 5.
  IF sy-subrc EQ 1.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Transporte' lv_tknum 'no existe.'
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ELSEIF sy-subrc NE 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    CONCATENATE 'Error obteniendo datos de cabecera' lv_tknum
      INTO les_return-msg SEPARATED BY space.
    APPEND les_return TO c_return.
  ENDIF.

  CHECK c_return IS INITIAL.

*  Mapeo de datos.
  APPEND les_vttkvb TO lti_vttkvb.
  les_vttkvb-stdis = 'X'.
  les_vttkvb-dtdis = sy-datum.
  les_vttkvb-uzdis = sy-uzeit.
*{   INSERT         ER3K900332                                        3

*  les_vttkvb-pkstk = 'X'.

*}   INSERT

*  Modificación de cabecera de transporte.
  CALL FUNCTION 'SD_SHIPMENT_HEADER_CHANGE'
*{   INSERT         ER3K900332                                        2
      EXPORTING
     OPT_DIALOG = ' '
*}   INSERT
    TABLES
      c_xvttk                            = lti_vttkvb
      c_yvttk                            = lti_yttkvb
      c_xvttp                            = lti_xvttp
      c_yvttp                            = lti_yvttp
      c_xvtts                            = lti_vttsvb
      c_yvtts                            = lti_vttsvb2
      c_xvtsp                            = lti_vtspvb
      c_yvtsp                            = lti_vtspvb2
      c_xvbpa                            = lti_vbpavb
      c_yvbpa                            = lti_vbpavb2
      c_xvbadr                           = lti_sadrvb
      c_yvbadr                           = lti_sadrvb
      i_xtrlk                            = lti_xtrlk
      i_xtrlp                            = lti_xtrlp
    CHANGING
      c_xvttk_new                        = les_vttkvb
   EXCEPTIONS
     invalid_change                     = 1
     route_insert_failed                = 2
     tdlnr_insert_failed                = 3
     status_planned_failed              = 4
     status_registrated_failed          = 5
     status_loading_start_failed        = 6
     status_loading_end_failed          = 7
     status_completion_failed           = 8
     status_shipment_start_failed       = 9
     status_shipment_end_failed         = 10
     OTHERS                             = 11
            .
  IF sy-subrc <> 0.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error modificando transporte'.
    APPEND les_return TO c_return.
  ENDIF.

  REFRESH lti_vttkvb[].
  APPEND les_vttkvb TO lti_vttkvb.

*    Guardar datos de cabecera de transporte y sus posiciones.
  CALL FUNCTION 'SD_SHIPMENTS_SAVE'
    EXPORTING
      i_transaktionstyp      = 'V'
      i_opt_update_task      = 'X'
    IMPORTING
      e_log_always           = lv_log_always
      e_log_on_error_warning = lv_log_on_error_warning
      e_save_log             = lv_save_log
    TABLES
      i_xvttk                = lti_vttkvb
      i_yvttk                = lti_yttkvb
      i_xvttp                = lti_xvttp
      i_yvttp                = lti_yvttp
      i_vtrlk                = lti_xtrlk
      i_vtrlp                = lti_xtrlp
    EXCEPTIONS
      no_change              = 1
      delivery_split_error   = 2
      OTHERS                 = 3.

  IF sy-subrc EQ 0.
*{   REPLACE        ER3K900332                                        1
*\    COMMIT WORK.
    COMMIT WORK AND WAIT.
*}   REPLACE
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'S'.
    les_return-msg = 'Transporte guardado correctamente'.
    APPEND les_return TO c_return.

  ELSE.
    les_return-num_doc = i_transporte.
    les_return-type_msg = 'E'.
    les_return-msg = 'Error guardando el transporte'.
    APPEND les_return TO c_return.
  ENDIF.

ENDMETHOD.
ENDCLASS.
