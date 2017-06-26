class ZCL_LGTICA_EMBALAJE definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_EMBALAJE
*"* do not include other source files here!!!
public section.

  class-data ENTREGA type VBELN_VL .
  data TRANSPORTE type TKNUM .

  class-methods GET_UNIDADES
    importing
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO optional
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL optional
    changing
      !C_UNIDADES type ZTTSD_PACKING_UNEM
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods PRINT_UNIDADES
    importing
      value(I_USUARIO) type ZED_USUARIO_MOVIL
      value(I_DET_CABECERA) type ZTTSD_CAB_REF
      value(I_MATERIAL_EMB) type MATNR
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_ENTREGA) type VBELN_VL optional
    changing
      value(C_RETURN) type ZTTSD_MSG_PACKING optional .
  class-methods CREAR_UM
    importing
      !I_MATERIAL type MATNR
      !I_WERKS type HUM_WERKS
      !I_TKNUM type TKNUM
      !I_TKNUM_SE type ZED_TKNUM optional
      !I_TIPO_DOCUMENTO type ZE_TIPO_DOC
      !I_USUARIO type ZED_USUARIO_MOVIL optional
    exporting
      !E_NUMERO_HU type EXIDV
      !E_NUMERO_HU_SE type ZED_VENUM
      !E_CONSECUTIVO type ZED_CONSECUTIVO
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods PACK_MATERIAL
    importing
      !I_ES_DETALLE type ZEDSD_PACKING_POS
      !I_ES_LOTE type ZEDSD_PACKING_LOT optional
      !I_TKNUM type TKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods VACIAR_UM
    importing
      !I_EXIDV type EXIDV
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods BORRAR_UM
    importing
      !I_EXIDV type EXIDV
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods GET_ROTULO
    importing
      !I_EXIDV type EXIDV
    exporting
      !E_ROTULO type ZED_COD_BAR .
  class-methods GET_ROTULO_SE
    importing
      !I_VENUM_SE type ZED_VENUM
      !I_DOCUMENTO type ZE_DOCUMENTO optional
      !I_TKNUM_SE type ZED_TKNUM optional
    exporting
      !E_ROTULO type ZED_COD_BAR .
  class-methods GET_UM_ROTULO
    importing
      !I_ROTULO type ZED_COD_BAR
    exporting
      !E_EXIDV type EXIDV .
  class-methods GET_UM_ROTULO_SE
    importing
      !I_ROTULO type ZED_COD_BAR
    exporting
      !E_EXIDV type EXIDV .
  class-methods UPDATE_UBICACION
    importing
      !I_ROTULO type ZED_COD_BAR
      !I_UBICACION type ZED_UBICACION_TMP
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods GET_LIST_ROTULOS
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
    exporting
      !E_TI_ROTULOS type ZTTSD_ROTULO_C
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods BORRAR_UM_SE
    importing
      !I_EXIDV type EXIDV
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods PACK_MATERIAL_SE
    importing
      !I_ES_DETALLE type ZEDSD_PACKING_POS
      !I_ES_LOTE type ZEDSD_PACKING_LOT optional
      !I_TKNUM type TKNUM
      !I_ENTREGA type VBELN_VL
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
protected section.
*"* protected components of class ZCL_LGTICA_EMBALAJE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_EMBALAJE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_EMBALAJE IMPLEMENTATION.


METHOD borrar_um.

  DATA: les_items TYPE bapihuitmunpack,
        les_vekp  TYPE vekp,
        lti_msg   TYPE zttsd_msg_packing.

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lv_venum   TYPE  venum,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing.

  CLEAR: les_mensajes.

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  SELECT SINGLE venum
    FROM vekp
    INTO lv_venum
    WHERE exidv EQ i_exidv.

  IF sy-subrc EQ 0.
    les_venum-venum = lv_venum.
    APPEND les_venum TO lti_venum.
  ELSE.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
    CONCATENATE 'No se encontraron datos de cabecera para unidad de manipulación'
       i_exidv INTO les_mensajes-msg SEPARATED BY space.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

**get HU details. this will fill the global tables.
  CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      if_lock_hus = 'X'
      it_venum    = lti_venum[]
    IMPORTING
      et_header   = lti_header
      et_items    = lti_huitem
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        1
*\    CONCATENATE 'Error obteniendo datos de unidad de manipulación' i_exidv
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S016(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

*  Vaciar unidad de manipulación.
  CALL FUNCTION 'HU_EMPTY_HU'
    EXPORTING
      if_exidv      = i_exidv
    EXCEPTIONS
      input_missing = 1
      hu_not_found  = 2
      no_contens    = 3
      not_possible  = 4
      fatal_error   = 5
      OTHERS        = 6.
  IF sy-subrc NE 0 AND sy-subrc NE 3.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        2
*\    CONCATENATE 'Error al vaciar unidad de manipulación' i_exidv
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S018(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

*  CALL FUNCTION 'HU_POST'
*    EXPORTING
*      if_synchron = 'X'
*      if_commit   = 'X'.

*  Eliminar unidad de manipulación.
  CALL FUNCTION 'HU_DELETE_HU'
    EXPORTING
      if_with_lower = 'X'
      if_venum      = lv_venum
    EXCEPTIONS
      not_found     = 1
      not_possible  = 2
      snr_problem   = 3
      fatal_error   = 4
      OTHERS        = 5.
  IF sy-subrc NE 0.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        3
*\    CONCATENATE 'Error eliminando unidad de manipulación' i_exidv
*\      INTO les_mensajes-msg SEPARATED BY space.

    MESSAGE S027(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

  CALL FUNCTION 'HU_POST'
    EXPORTING
      if_synchron = 'X'
      if_commit   = 'X'.

  COMMIT WORK AND WAIT.

  les_mensajes-num_doc = i_exidv.
  les_mensajes-type_msg = 'S'.
*{   REPLACE        ER3K900332                                        4
*\  CONCATENATE 'Se ha eliminado unidad de manipulación' i_exidv
*\    INTO les_mensajes-msg SEPARATED BY space.
MESSAGE S028(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
  APPEND les_mensajes TO c_mensajes.

*  Eliminar datos de tabla Z de packing.
  DELETE FROM zmm_t_transp_dt WHERE venum EQ lv_venum.

ENDMETHOD.


  method BORRAR_UM_SE.
*{   INSERT         ER3K900332                                        1
  DATA: les_items TYPE bapihuitmunpack,
        les_vekp  TYPE vekp,
        lti_msg   TYPE zttsd_msg_packing.

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lv_venum   TYPE  venum,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing.

  CLEAR: les_mensajes.

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  SELECT SINGLE venum
    FROM ZMM_T_TRANSP_DT
    INTO lv_venum
    WHERE exidv EQ i_exidv.

  IF sy-subrc EQ 0.
    les_venum-venum = lv_venum.
    APPEND les_venum TO lti_venum.
  ELSE.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
    CONCATENATE 'No se encontraron datos de cabecera para unidad de manipulación'
       i_exidv INTO les_mensajes-msg SEPARATED BY space.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.


*  Eliminar datos de tabla Z de packing.
  DELETE FROM zmm_t_transp_dt WHERE EXIDV EQ i_EXIDV.
  les_mensajes-num_doc = i_exidv.
  les_mensajes-type_msg = 'S'.
  CONCATENATE 'Se ha eliminado unidad de manipulación' i_exidv
    INTO les_mensajes-msg SEPARATED BY space.
  APPEND les_mensajes TO c_mensajes.

  COMMIT WORK AND WAIT.

*}   INSERT
  endmethod.


METHOD crear_um.

  DATA: les_proposal TYPE bapihuhdrproposal,
        les_header   TYPE bapihuheader,
        les_huheader TYPE bapihuheader,
        les_creturn  TYPE zesd_msg_packing,
        les_lips     TYPE lips,
        lv_exid      TYPE exidv.

  DATA: lti_itempro  TYPE TABLE OF bapihuitmproposal,
        les_itempro  LIKE LINE OF  lti_itempro,
        lti_itemser  TYPE TABLE OF bapihuitmserialno,
        les_itemser  LIKE LINE OF  lti_itemser,
        lti_return   TYPE TABLE OF bapiret2,
        les_return   LIKE LINE OF lti_return,
        lti_huitem   TYPE TABLE OF bapihuitem,
        les_huitem   LIKE LINE OF  lti_huitem.

  DATA: lv_tknum TYPE tknum,
        lv_vbeln TYPE vbeln.

"Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDUNMANIP', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado


*  Variables para el guardado en tablas Z de packing.
  DATA: lti_transp_dt TYPE TABLE OF zmm_t_transp_dt,
        les_transp_dt TYPE zmm_t_transp_dt,
        lv_subrc      LIKE sy-subrc.

CASE i_tipo_documento.
*{   REPLACE        ER3K900279                                        6
*\  WHEN 'ENTR'.
  WHEN 'ENTR' OR 'RINV' OR 'OSIN' OR 'OPSG'.
*}   REPLACE
*  Mapeo de datos.
  les_proposal-pack_mat = i_material.
  les_proposal-plant = i_werks.
  les_proposal-hu_status_init = 'A'.
*{   INSERT         ER3K900279                                        3
*  Modificamos la cabecera para indicar que es un transporte
*  y asociar la unidad de manipulacion con el numero de transporte.
  les_header-pack_mat_object = '04'.
  les_header-pack_mat_obj_key = i_tknum.

*  Sleccionar el canal de distribución
  SELECT SINGLE vtweg
    FROM mvke
    INTO les_header-dc_custom_mat
    WHERE matnr EQ les_proposal-pack_mat.

*}   INSERT

*  Funcion para crear unidad de manipulación.
  CALL FUNCTION 'BAPI_HU_CREATE'
    EXPORTING
      headerproposal = les_proposal
    IMPORTING
      huheader       = les_header
      hukey          = lv_exid
    TABLES
      itemsproposal  = lti_itempro
      itemsserialno  = lti_itemser
      return         = lti_return
      huitem         = lti_huitem.

*  Si retorna mensaje de error llenamos tabla de mensajes.
  IF lti_return IS NOT INITIAL.
    LOOP AT lti_return INTO les_return.
      les_creturn-num_doc = les_return-message_v1.
      les_creturn-msg = les_return-message.
      les_creturn-type_msg = les_return-type.
      APPEND les_creturn TO c_mensajes.
    ENDLOOP.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  CHECK lti_return IS INITIAL.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

*  Modificamos la cabecera para indicar que es un transporte
*  y asociar la unidad de manipulacion con el numero de transporte.
  les_header-pack_mat_object = '04'.
  les_header-pack_mat_obj_key = i_tknum.

*  Sleccionar el canal de distribución
  SELECT SINGLE vtweg
    FROM mvke
    INTO les_header-dc_custom_mat
    WHERE matnr EQ les_proposal-pack_mat.


* Cambiar cabecera de unidad de manipulación.
  CALL FUNCTION 'BAPI_HU_CHANGE_HEADER'
    EXPORTING
      hukey     = lv_exid
      huchanged = les_header
    IMPORTING
      huheader  = les_huheader
    TABLES
      return    = lti_return.

*  Si hay algun error llenamos los mensajes de retorno.
  IF lti_return IS NOT INITIAL.
    LOOP AT lti_return INTO les_return.
      les_creturn-num_doc = les_return-message_v1.
      les_creturn-msg = les_return-message.
      les_creturn-type_msg = les_return-type.
      APPEND les_creturn TO c_mensajes.
    ENDLOOP.
*    Hacemos rollback de la bapi
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
*  Verificamos que no halla errores.
  CHECK lti_return IS INITIAL.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  e_numero_hu = lv_exid.

  les_creturn-num_doc = i_tknum.
*{   REPLACE        ER3K900279                                        7
*\  les_creturn-msg = 'Unidad de Manipulación creada correctamente'.
  MESSAGE S014(ZCLPACK) into les_creturn-msg.
*}   REPLACE
  les_creturn-type_msg = 'S'.
  APPEND les_creturn TO c_mensajes.

*  Obtener el consecutivo que se guarda en tabla de detalles de packing.
  CALL METHOD zcl_lgtica_util=>get_consecutivo
    EXPORTING
      i_tknum       = i_tknum
    IMPORTING
      e_consecutivo = les_transp_dt-consecutivo.

*  consecutivo para unidades de manipulación.
  e_consecutivo = les_transp_dt-consecutivo.
*{   INSERT         ER3K900279                                        4
  les_transp_dt-tknum_se = i_tknum.
*}   INSERT

*  Mapeo de datos a guardar en tablas de detalles de packing.
  les_transp_dt-tknum = i_tknum.
  les_transp_dt-venum = les_header-hu_id.
  les_transp_dt-exidv = lv_exid.
  les_transp_dt-fecha = sy-datum.
  les_transp_dt-hora  = sy-uzeit.
*  les_transp_dt-usuario = i_usuario.
  APPEND les_transp_dt TO lti_transp_dt.

*  Guardar datos en tablas de packing
  CALL METHOD zcl_lgtica_util=>modify_zmm_t_transp_dt
    EXPORTING
      i_ti_transp_dt  = lti_transp_dt
      i_mod           = 'I'
    IMPORTING
*      e_dbcnt         =
      e_subrc         = lv_subrc.
*{   INSERT         ER3K900279                                        1
  COMMIT WORK AND WAIT.
*}   INSERT

  WHEN OTHERS.
*    Crear unidad de manipulacion solo guardando en tablas z.
            CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = wnorange
            object                  = wobjeto
            subobject               = wsubobj
          IMPORTING
            number                  = p_doc_number " Número generado por SAP
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            internal_overflow       = 6
            OTHERS                  = 7.

            les_transp_dt-VENUM_SE = p_doc_number.


*  Obtener el consecutivo que se guarda en tabla de detalles de packing.
  CALL METHOD zcl_lgtica_util=>get_consecutivo
    EXPORTING
      i_tknum_se       = i_tknum_se
    IMPORTING
      e_consecutivo = les_transp_dt-consecutivo.

*  consecutivo para unidades de manipulación.
  e_consecutivo = les_transp_dt-consecutivo.

*  Mapeo de datos a guardar en tablas de detalles de packing.
*{   REPLACE        ER3K900279                                        5
*\  les_transp_dt-tknum = i_tknum.
  les_transp_dt-tknum = i_tknum_se+8(10).
*}   REPLACE
  les_transp_dt-tknum_se = i_tknum_se.
*  les_transp_dt-VENUM_SE = les_header-hu_id.

  les_transp_dt-exidv = les_transp_dt-VENUM_SE.
  les_transp_dt-fecha = sy-datum.
  les_transp_dt-hora  = sy-uzeit.
*  les_transp_dt-usuario = i_usuario.
  APPEND les_transp_dt TO lti_transp_dt.

*  Guardar datos en tablas de packing
  CALL METHOD zcl_lgtica_util=>modify_zmm_t_transp_dt
    EXPORTING
      i_ti_transp_dt  = lti_transp_dt
      i_mod           = 'I'
    IMPORTING
*      e_dbcnt         =
      e_subrc         = lv_subrc.
*{   INSERT         ER3K900279                                        2
COMMIT WORK AND WAIT.
*}   INSERT

ENDCASE.


ENDMETHOD.


METHOD GET_LIST_ROTULOS.
*.... Variable para ajustar número de documento
  DATA: l_e_documento TYPE ze_documento.
  DATA: lv_tknum TYPE tknum,
        lv_tknum_se TYPE zed_tknum,
        lti_rotulos_p TYPE TABLE OF zedsd_rotulo,
        les_rotulos_p LIKE LINE OF lti_rotulos_p,
        les_rotulos   TYPE zedsd_rotulo_c ,
        les_mensajes  TYPE zesd_msg_packing,
        lti_transp_dt TYPE TABLE OF zmm_t_transp_dt,
        les_transp_dt TYPE zmm_t_transp_dt.

*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = i_documento
          IMPORTING
            output = l_e_documento.

*  Verificamos que la entrega tenga asociada un transporte.
  SELECT SINGLE tknum tknum_se
    FROM zmm_t_transporte
    INTO (lv_tknum ,lv_tknum_se)
    WHERE tipo_doc EQ i_tipo_doc
      AND documento EQ l_e_documento.

  IF sy-subrc EQ 0.
*    Buscamos en las posiciones de packing las posiciones del transporte.
*{   REPLACE        ER3K900332                                        2
*\    SELECT *
*\      FROM zmm_t_transp_dt
*\      INTO CORRESPONDING FIELDS OF TABLE lti_transp_dt
    SELECT MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
      FROM zmm_t_transp_dt
      INTO TABLE lti_transp_dt
*}   REPLACE
      WHERE tknum EQ lv_tknum and tknum_se eq lv_tknum_se.
    IF sy-subrc EQ 0.
*      En caso de encontrarse posiciones se agregan los rotulos a la
*      tabla de retorno de rotulos.
      LOOP AT lti_transp_dt INTO les_transp_dt.
        CLEAR les_rotulos.
        IF les_transp_dt-tknum is not INITIAL.
          CONCATENATE les_transp_dt-tknum les_transp_dt-consecutivo
          INTO les_rotulos-rotulo.
        ELSEIF les_transp_dt-tknum_se is not INITIAL.
*{   REPLACE        ER3K900332                                        1
*\            CONCATENATE les_transp_dt-tknum_se les_transp_dt-consecutivo
            CONCATENATE les_transp_dt-tknum_se+8(10) les_transp_dt-consecutivo
*}   REPLACE
          INTO les_rotulos-rotulo.
        ENDIF.

        les_rotulos-ubicacion = les_transp_dt-ubicacion.
        APPEND les_rotulos TO e_ti_rotulos.
      ENDLOOP.
    ENDIF.

  ELSE.

*    En caso de que no halla datos en tabla de packing se agrega error.
    les_mensajes-num_doc = i_documento.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        3
*\    les_mensajes-msg = 'Entrega no tiene rotulos asociados'.
    MESSAGE S031(ZCLPACK) INTO les_mensajes-msg .
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

ENDMETHOD.


METHOD get_rotulo.

  DATA: lv_tknum TYPE tknum,
        lv_consecutivo TYPE zed_consecutivo.

  SELECT SINGLE tknum consecutivo
    FROM zmm_t_transp_dt
    INTO (lv_tknum, lv_consecutivo)
    WHERE exidv EQ i_exidv.

  IF sy-subrc EQ 0.
    CONCATENATE lv_tknum lv_consecutivo
      INTO e_rotulo.
  ENDIF.

ENDMETHOD.


METHOD GET_ROTULO_SE.

  DATA: lv_tknum TYPE ZED_TKNUM,
        lv_consecutivo TYPE zed_consecutivo.

*{   REPLACE        ER3K900340                                        1
*\  SELECT SINGLE tknum_se consecutivo
*\    FROM zmm_t_transp_dt
*\    INTO (lv_tknum, lv_consecutivo)
*\    WHERE VENUM_SE EQ i_VENUM_SE.
  IF i_documento is INITIAL.

  SELECT SINGLE tknum_se consecutivo
    FROM zmm_t_transp_dt
    INTO (lv_tknum, lv_consecutivo)
    WHERE tknum_se eq I_TKNUM_SE
    and CONSECUTIVO EQ i_VENUM_SE.
  ELSE.

    SELECT SINGLE tknum_se
    FROM zmm_t_transporte
    INTO lv_tknum
    WHERE documento eq I_documento.

  ENDIF.

*}   REPLACE

  IF sy-subrc EQ 0.
    CONCATENATE lv_tknum lv_consecutivo
      INTO e_rotulo.
  ENDIF.

ENDMETHOD.


METHOD get_um_rotulo.

  DATA: les_rotulo TYPE zedsd_rotulo.

  les_rotulo = i_rotulo.

  SELECT SINGLE exidv
    FROM zmm_t_transp_dt
    INTO e_exidv
    WHERE tknum EQ les_rotulo-tknum
      AND consecutivo EQ les_rotulo-consecutivo.

ENDMETHOD.


METHOD GET_UM_ROTULO_SE.

  DATA: les_rotulo TYPE zedsd_rotulo_se.
*{   REPLACE        ER3K900332                                        1
*\
*\  les_rotulo-tknum_se = i_rotulo.

  les_rotulo-tknum_se = i_rotulo(10).
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = les_rotulo-tknum_se
   IMPORTING
     OUTPUT        = les_rotulo-tknum_se
            .

*}   REPLACE
  les_rotulo-consecutivo = i_rotulo.
  SELECT SINGLE exidv
    FROM zmm_t_transp_dt
    INTO e_exidv
    WHERE tknum_Se EQ les_rotulo-tknum_se
      AND consecutivo EQ les_rotulo-consecutivo.

ENDMETHOD.


METHOD get_unidades.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Consulta Unidades de Embalaje
* Autor Prog.  :
* Fecha Creac. : 23.10.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*

*.....Typo para Tabla mara
  TYPES : BEGIN OF ltp_mara,
           matnr TYPE matnr,    "material
           mtart TYPE mtart,    "tipo de material
           meins TYPE meins,    "Unidad de medida base
           brgew TYPE brgew,    "Peso bruto
           ntgew TYPE ntgew,    "Peso neto
           gewei TYPE gewei,    "Unidad de peso
           volum TYPE volum,    "Volumen
           voleh TYPE voleh,    "Unidad de volumen
           laeng TYPE laeng,    "Longitud
           breit TYPE breit,    "Ancho
           hoehe TYPE hoehe,    "Altura
           meabm TYPE meabm,    "Unidad dimensión
           ERGEW type ERGEW,
         END OF ltp_mara.
TYPES: BEGIN OF ltp_makt,
          matnr type matnr,
          maktx type maktx,
       END of ltp_makt.

  DATA:
*.... Tabla interna del ZEDSD_PACKING_UNEM unidades de embalaje
        lti_unidades TYPE TABLE OF ZEDSD_PACKING_UNEM,
        les_unidades TYPE  ZEDSD_PACKING_UNEM,
*.... Tabla interna del tipo ltp_mara
        lti_mara TYPE TABLE OF ltp_mara,
        les_mara TYPE  ltp_mara,
*.... Tabla interna del tipo ltp_makt
        lti_makt TYPE TABLE OF ltp_makt,
        les_makt TYPE  ltp_makt,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable Estructura para Mensajes
        les_msg TYPE zesd_msg_picking.


*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.
*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*... seleccciono los materiales con tipo de material embalaje y clase de embalaje
    Select matnr mtart meins brgew ntgew gewei volum voleh laeng breit hoehe meabm
      into table lti_mara
      from mara
      where MTART eq 'VERP' and
      LVORM eq ' ' and
      VHART ne ' '.

      IF sy-subrc eq 0.
*.... Consulto la descripción de los materiales
          select matnr maktx
            into table lti_makt
            from makt
            for all entries in lti_mara
            where matnr eq lti_mara-matnr and
            spras eq sy-langu.
*.... Recorro los materiales encontrados y los agrego a la estructura de unidades de embalaje de salida
        LOOP AT lti_mara into les_mara.
          clear: les_unidades.
          move-corresponding les_mara to les_unidades.
          READ TABLE lti_makt into les_makt with key matnr = les_mara-matnr.
          IF sy-subrc eq 0.
             les_unidades-maktx =  les_makt-maktx.
          ENDIF.
          APPEND les_unidades to c_unidades.
        ENDLOOP.
      ENDIF.
  ENDIF.
ENDMETHOD.


METHOD pack_material.

  DATA: les_huitem    TYPE huitem_from,
        les_proposal  TYPE huitm_proposal,
        les_item      TYPE vepovb,
        les_lips      TYPE lips,
        lti_messagges TYPE huitem_messages_t,
        les_header    TYPE vekpvb,
        les_ser       TYPE zedsd_packing_ser,
        les_sernr     TYPE e1rmsno.

  DATA: les_packing   TYPE packing_item_hu,
        lv_subrc      LIKE sy-subrc,
        lv_venum      TYPE venum,
        lv_matnr      type matnr,
        les_request   TYPE packing_item_hu.

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing.

*{   INSERT         ER3K900332                                        5
DATA:    lvs type integer,
         I_MATNR type  MARA-MATNR,
         I_IN_ME type  MARA-MEINS,
         I_OUT_ME type  MARA-MEINS,
         I_MENGE type  EKPO-MENGE,
         E_MENG type  EKPO-MENGE.
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
*}   INSERT

*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_es_detalle-material
        IMPORTING
          output = lv_matnr.

*  Obtener datos de material a embalar.
  IF i_es_lote IS INITIAL.
*{   REPLACE        ER3K900332                                        1
*\    SELECT SINGLE *
*\      FROM lips
*\      INTO CORRESPONDING FIELDS OF les_lips
*\      WHERE vbeln = i_es_detalle-vbeln2
*\      AND   matnr = lv_matnr.
    SELECT SINGLE *
      FROM lips
      INTO CORRESPONDING FIELDS OF les_lips
      WHERE vbeln = i_es_detalle-vbeln2
      AND   matnr = lv_matnr
      and POSNR = i_es_detalle-POSICION.
*}   REPLACE
  ELSE.
*{   REPLACE        ER3K900332                                        2
*\    SELECT SINGLE *
*\      FROM lips
*\      INTO CORRESPONDING FIELDS OF les_lips
*\      WHERE vbeln = i_es_detalle-vbeln2
*\      AND   matnr = lv_matnr
*\      AND   charg = i_es_lote-lote.
    SELECT SINGLE *
      FROM lips
      INTO CORRESPONDING FIELDS OF les_lips
      WHERE vbeln = i_es_detalle-vbeln2
      AND   matnr = lv_matnr
      AND   charg = i_es_lote-lote
      and UECHA = i_es_detalle-POSICION.
      IF sy-subrc ne 0.
      SELECT SINGLE *
      FROM lips
      INTO CORRESPONDING FIELDS OF les_lips
      WHERE vbeln = i_es_detalle-vbeln2
      AND   matnr = lv_matnr
      AND   charg = i_es_lote-lote
      and   POSNR = i_es_detalle-POSICION.
      ENDIF.
*}   REPLACE
  ENDIF.


  IF sy-subrc EQ 0.
*    Mapeo de datos para estructura de packing.
    MOVE-CORRESPONDING les_lips TO les_packing.

  ELSE.
*    En caso de no encontrar datos llenar mensajes
    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        8
*\    CONCATENATE 'Material' i_es_detalle-material ' no esta asociada a entrega' i_es_detalle-vbeln2
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S021(ZCLPACK) WITH i_es_detalle-material i_es_detalle-vbeln2 into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

  IF i_es_lote IS NOT INITIAL.
*    Mapeo de datos lote.
    les_packing-velin    = '1'.
    les_packing-belnr    = i_es_lote-vbeln2.
*{   REPLACE        ER3K900332                                        3
*\    les_packing-quantity = i_es_lote-cantidad.
      IF les_lips-meins NE les_lips-vrkme.
*************
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = les_lips-meins
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
            EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = les_lips-meins.
* Implement suitable error handling here
             ENDIF.

              I_MATNR = lv_matnr.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = les_lips-vrkme.
              I_MENGE = i_es_detalle-cantidad.

             CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
               EXPORTING
                 I_MATNR                    = I_MATNR
                 I_IN_ME                    = I_IN_ME
                 I_OUT_ME                   = I_OUT_ME
                 I_MENGE                    = I_MENGE
              IMPORTING
                E_MENGE                    = E_MENG
              EXCEPTIONS
                ERROR_IN_APPLICATION       = 1
                ERROR                      = 2
                OTHERS                     = 3
                       .
             IF SY-SUBRC <> 0.
* Implement suitable error handling here
             ENDIF.
*           IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
*             les_vbpok_lote-lfimg = FLOOR( E_MENG ).
*           ELSE.
*             les_vbpok_lote-lfimg = E_MENG.
*           ENDIF.

                 IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.

                    lvs = ( E_MENG * 1000 ) MOD 10 .
                    IF  lvs >= 1  AND lvs < 5.
                        E_MENG = E_MENG -  ( lvs / 1000 ).
                    ELSEIF lvs > 4  AND lvs <= 9 .
                        E_MENG = E_MENG + ( ( 10 - lvs ) / 1000 ).

                    ENDIF.

                 ENDIF.

                 les_packing-quantity = E_MENG.
************
*        les_packing-quantity = i_es_detalle-cantidad * les_lips-UMVKN / les_lips-UMVKZ  .
      ELSE.
        les_packing-quantity = i_es_detalle-cantidad .
      ENDIF.

*}   REPLACE
    les_packing-charg    = i_es_lote-lote.
    les_packing-pstyv    = ''.
    les_packing-meins    = i_es_lote-umc.
  ELSE.
*    Mapeo de datos
    les_packing-velin    = '1'.
    les_packing-belnr    = i_es_detalle-vbeln2.
*{   REPLACE        ER3K900332                                        7
*\    les_packing-quantity = i_es_detalle-cantidad.
    IF les_lips-meins NE les_lips-vrkme.
*        les_packing-quantity = i_es_detalle-cantidad * les_lips-UMVKN / les_lips-UMVKZ  .
***********
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = les_lips-meins
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
            EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = les_lips-meins.
* Implement suitable error handling here
             ENDIF.

              I_MATNR = lv_matnr.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = les_lips-vrkme.
              I_MENGE = i_es_detalle-cantidad.

             CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
               EXPORTING
                 I_MATNR                    = I_MATNR
                 I_IN_ME                    = I_IN_ME
                 I_OUT_ME                   = I_OUT_ME
                 I_MENGE                    = I_MENGE
              IMPORTING
                E_MENGE                    = E_MENG
              EXCEPTIONS
                ERROR_IN_APPLICATION       = 1
                ERROR                      = 2
                OTHERS                     = 3
                       .
             IF SY-SUBRC <> 0.
* Implement suitable error handling here
             ENDIF.
*           IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
*             les_vbpok_lote-lfimg = FLOOR( E_MENG ).
*           ELSE.
*             les_vbpok_lote-lfimg = E_MENG.
*           ENDIF.

                 IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.

                    lvs = ( E_MENG * 1000 ) MOD 10 .
                    IF  lvs >= 1  AND lvs < 5.
                        E_MENG = E_MENG -  ( lvs / 1000 ).
                    ELSEIF lvs > 4  AND lvs <= 9 .
                        E_MENG = E_MENG + ( ( 10 - lvs ) / 1000 ).

                    ENDIF.

                 ENDIF.

                 les_packing-quantity = E_MENG.
***********
      ELSE.
        les_packing-quantity = i_es_detalle-cantidad .
      ENDIF.
*}   REPLACE
    les_packing-pstyv    = ''.
    les_packing-meins    = i_es_detalle-umc.

  ENDIF.

*  Obtener datos del transporte.
*{   REPLACE        ER3K900332                                        6
*\  CALL FUNCTION 'RV_SHIPMENT_VIEW'
*\    EXPORTING
*\      shipment_number       = i_tknum
*\      option_items          = 'X'
*\      option_package_dialog = 'X'
*\      option_partners       = 'X'
*\    EXCEPTIONS
*\      not_found             = 1
*\      no_authority          = 2
*\      delivery_missing      = 3
*\      delivery_lock         = 4
*\      OTHERS                = 5.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number       = i_tknum
      option_items          = 'X'
      option_package_dialog = 'X'
      option_partners       = 'X'
      OPTION_DELIVERY_LOCK  = 'X'
      OPTION_TVTK  = 'X'
      OPTION_TTDS = 'X'
      OPTION_FLOW = 'X'
     IMPORTING

      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
    EXCEPTIONS
      not_found             = 1
      no_authority          = 2
      delivery_missing      = 3
      delivery_lock         = 4
      OTHERS                = 5.
*}   REPLACE
  IF sy-subrc NE 0.
*    En caso de alguna excepcion se llena tabla de errores.
    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        9
*\    CONCATENATE 'No se pueden obtener datos del transporte' i_tknum
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S022(ZCLPACK) WITH i_tknum into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

**refrech buffer
*  CALL FUNCTION 'HU_INITIALIZE_PACKING'.

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  SELECT SINGLE venum
    FROM vekp
    INTO lv_venum
    WHERE exidv EQ i_es_detalle-umanipulacion.

*  En caso de que la unidad de manipulación exista se mapea,
*  de lo contrario se llena mensajes de error.
  IF sy-subrc EQ 0.

    les_venum-venum = lv_venum.
    APPEND les_venum TO lti_venum.

  ELSE.

    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       10
*\    CONCATENATE 'Unidad de manipulación con identificador externo'
*\                i_es_detalle-umanipulacion 'no existe'
*\      INTO les_mensajes-msg SEPARATED BY space.
      MESSAGE S023(ZCLPACK) WITH i_es_detalle-umanipulacion  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.
*  Mapear datos de items.
  les_packing-venum = lv_venum.
  les_packing-exidv = i_es_detalle-umanipulacion.

* Obtener detalles de unidades de manipulación. Esta funcion
* llena las tablas globales del grupo de funciones.
  CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      if_lock_hus = 'X'
      it_venum    = lti_venum[]
    IMPORTING
      et_header   = lti_header
      et_items    = lti_huitem
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
*    En caso de error se llena tabla con mensajes de  error.
    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       11
*\    CONCATENATE 'No se puede obtener datos de unidad de manipulación'
*\                i_es_detalle-umanipulacion
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S024(ZCLPACK) WITH i_es_detalle-umanipulacion  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

*  Funcion para empacar material.
  CALL FUNCTION 'HU_PACKING_AND_UNPACKING'
    EXPORTING
      if_repack          = ' '
      is_packing_request = les_packing
    IMPORTING
      ef_rcode           = lv_subrc
      es_p_request       = les_request
      es_item            = les_item
    CHANGING
      cs_header          = les_header
    EXCEPTIONS
      missing_data       = 1
      hu_not_changeable  = 2
      not_possible       = 3
      customizing        = 4
      weight             = 5
      volume             = 6
      serial_nr          = 7
      fatal_error        = 8
      OTHERS             = 9.
  IF sy-subrc <> 0.
*    En caso de error se llena tabla con mensajes de  error.
    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       12
*\    CONCATENATE 'Error embalando material' i_es_detalle-material
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S025(ZCLPACK) WITH i_es_detalle-material  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

  CALL FUNCTION 'HU_POST'.

  COMMIT WORK AND WAIT.

ENDMETHOD.


  method PACK_MATERIAL_SE.
*{   INSERT         ER3K900332                                        1


  DATA: les_huitem    TYPE huitem_from,
        les_proposal  TYPE huitm_proposal,
        les_item      TYPE vepovb,
        les_lips      TYPE lips,
        lti_messagges TYPE huitem_messages_t,
        les_header    TYPE vekpvb,
        les_ser       TYPE zedsd_packing_ser,
        les_sernr     TYPE e1rmsno.

  DATA: les_packing   TYPE packing_item_hu,
        lv_subrc      LIKE sy-subrc,
        lv_venum      TYPE venum,
        lv_matnr      type matnr,
        les_request   TYPE packing_item_hu.

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing.

*{   INSERT         ER3K900332                                        5
DATA:    lvs type integer,
         I_MATNR type  MARA-MATNR,
         I_IN_ME type  MARA-MEINS,
         I_OUT_ME type  MARA-MEINS,
         I_MENGE type  EKPO-MENGE,
         E_MENG type  EKPO-MENGE.
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
*}   INSERT

*{   REPLACE        ER3K900332                                        2
*\*.....Función para Ajuste de Número
*\      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\        EXPORTING
*\          input  = i_es_detalle-material
*\        IMPORTING
*\          output = lv_matnr.
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_es_detalle-material
        IMPORTING
          output = lv_matnr.

*      i_es_detalle-vbeln2 = i_entrega.
*}   REPLACE

*  Obtener datos de material a embalar.
*{   DELETE         ER3K900332                                        3
*\  IF i_es_lote IS INITIAL.
*}   DELETE
*{   REPLACE        ER3K900332                                        1
*\    SELECT SINGLE *
*\      FROM lips
*\      INTO CORRESPONDING FIELDS OF les_lips
*\      WHERE vbeln = i_es_detalle-vbeln2
*\      AND   matnr = lv_matnr.

    SELECT SINGLE *
      FROM lips
      INTO CORRESPONDING FIELDS OF les_lips
      WHERE vbeln = i_entrega
      AND   matnr = 'MATERIALES'
      and ARKTX = lv_matnr
      and VGPOS = i_es_detalle-umanipulacion_se(6)."i_es_detalle-pospack.
*}   REPLACE
*{   DELETE         ER3K900332                                        4
*\  ELSE.
*}   DELETE
*{   REPLACE        ER3K900332                                        2
*\    SELECT SINGLE *
*\      FROM lips
*\      INTO CORRESPONDING FIELDS OF les_lips
*\      WHERE vbeln = i_es_detalle-vbeln2
*\      AND   matnr = lv_matnr
*\      AND   charg = i_es_lote-lote.
*    SELECT SINGLE *
*      FROM lips
*      INTO CORRESPONDING FIELDS OF les_lips
*      WHERE vbeln = i_es_detalle-vbeln2
*      AND   matnr = 'MATERIALES'
*      and ARKTX = lv_matnr
*      AND   charg = i_es_lote-lote
*      and UECHA = i_es_detalle-POSICION.
*      IF sy-subrc ne 0.
*      SELECT SINGLE *
*      FROM lips
*      INTO CORRESPONDING FIELDS OF les_lips
*      WHERE vbeln = i_es_detalle-vbeln2
*      AND   matnr = lv_matnr
*      AND   charg = i_es_lote-lote
*      and   POSNR = i_es_detalle-POSICION.
*      ENDIF.
*}   REPLACE
*{   DELETE         ER3K900332                                        5
*\  ENDIF.
*}   DELETE


  IF sy-subrc EQ 0.
*    Mapeo de datos para estructura de packing.
    MOVE-CORRESPONDING les_lips TO les_packing.

  ELSE.
*    En caso de no encontrar datos llenar mensajes
*{   REPLACE        ER3K900332                                       10
*\    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-num_doc = i_entrega.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       19
*\    CONCATENATE 'Material' i_es_detalle-material ' no esta asociada a entrega' i_es_detalle-vbeln2
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S021(ZCLPACK) WITH i_es_detalle-material i_entrega into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.
*{   DELETE         ER3K900332                                        6
*\
*\  IF i_es_lote IS NOT INITIAL.
*\*    Mapeo de datos lote.
*\    les_packing-velin    = '1'.
*\    les_packing-belnr    = i_es_lote-vbeln2.
*\*{   REPLACE        ER3K900332                                        3
*\*\    les_packing-quantity = i_es_lote-cantidad.
*\      IF les_lips-meins NE les_lips-vrkme.
*\*************
*\        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*\             EXPORTING
*\               INPUT                = les_lips-meins
*\              LANGUAGE             = SY-LANGU
*\            IMPORTING
*\              OUTPUT               = I_IN_ME
*\            EXCEPTIONS
*\                UNIT_NOT_FOUND       = 1
*\                OTHERS               = 2.
*\             IF SY-SUBRC <> 0.
*\               I_IN_ME = les_lips-meins.
*\* Implement suitable error handling here
*\             ENDIF.
*\
*\              I_MATNR = lv_matnr.
*\*              I_IN_ME = <lfs_picking_det>-umc.
*\              I_OUT_ME = les_lips-vrkme.
*\              I_MENGE = i_es_detalle-cantidad.
*\
*\             CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*\               EXPORTING
*\                 I_MATNR                    = I_MATNR
*\                 I_IN_ME                    = I_IN_ME
*\                 I_OUT_ME                   = I_OUT_ME
*\                 I_MENGE                    = I_MENGE
*\              IMPORTING
*\                E_MENGE                    = E_MENG
*\              EXCEPTIONS
*\                ERROR_IN_APPLICATION       = 1
*\                ERROR                      = 2
*\                OTHERS                     = 3
*\                       .
*\             IF SY-SUBRC <> 0.
*\* Implement suitable error handling here
*\             ENDIF.
*\*           IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
*\*             les_vbpok_lote-lfimg = FLOOR( E_MENG ).
*\*           ELSE.
*\*             les_vbpok_lote-lfimg = E_MENG.
*\*           ENDIF.
*\
*\                 IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
*\
*\                    lvs = ( E_MENG * 1000 ) MOD 10 .
*\                    IF  lvs >= 1  AND lvs < 5.
*\                        E_MENG = E_MENG -  ( lvs / 1000 ).
*\                    ELSEIF lvs > 4  AND lvs <= 9 .
*\                        E_MENG = E_MENG + ( ( 10 - lvs ) / 1000 ).
*\
*\                    ENDIF.
*\
*\                 ENDIF.
*\
*\                 les_packing-quantity = E_MENG.
*\************
*\*        les_packing-quantity = i_es_detalle-cantidad * les_lips-UMVKN / les_lips-UMVKZ  .
*\      ELSE.
*\        les_packing-quantity = i_es_detalle-cantidad .
*\      ENDIF.
*\
*\*}   REPLACE
*\    les_packing-charg    = i_es_lote-lote.
*\    les_packing-pstyv    = ''.
*\    les_packing-meins    = i_es_lote-umc.
*\  ELSE.
*}   DELETE
*    Mapeo de datos
    les_packing-velin    = '1'.
*{   REPLACE        ER3K900332                                        8
*\    les_packing-belnr    = i_es_detalle-vbeln2.
    les_packing-belnr    = i_entrega.
*}   REPLACE
*{   REPLACE        ER3K900332                                        7
*\    les_packing-quantity = i_es_detalle-cantidad.
    IF les_lips-meins NE les_lips-vrkme.
*        les_packing-quantity = i_es_detalle-cantidad * les_lips-UMVKN / les_lips-UMVKZ  .
***********
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = les_lips-meins
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
            EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = les_lips-meins.
* Implement suitable error handling here
             ENDIF.

              I_MATNR = lv_matnr.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = les_lips-vrkme.
              I_MENGE = i_es_detalle-cantidad.

             CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
               EXPORTING
                 I_MATNR                    = I_MATNR
                 I_IN_ME                    = I_IN_ME
                 I_OUT_ME                   = I_OUT_ME
                 I_MENGE                    = I_MENGE
              IMPORTING
                E_MENGE                    = E_MENG
              EXCEPTIONS
                ERROR_IN_APPLICATION       = 1
                ERROR                      = 2
                OTHERS                     = 3
                       .
             IF SY-SUBRC <> 0.
* Implement suitable error handling here
             ENDIF.
*           IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
*             les_vbpok_lote-lfimg = FLOOR( E_MENG ).
*           ELSE.
*             les_vbpok_lote-lfimg = E_MENG.
*           ENDIF.

                 IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.

                    lvs = ( E_MENG * 1000 ) MOD 10 .
                    IF  lvs >= 1  AND lvs < 5.
                        E_MENG = E_MENG -  ( lvs / 1000 ).
                    ELSEIF lvs > 4  AND lvs <= 9 .
                        E_MENG = E_MENG + ( ( 10 - lvs ) / 1000 ).

                    ENDIF.

                 ENDIF.

                 les_packing-quantity = E_MENG.
***********
      ELSE.
        les_packing-quantity = i_es_detalle-cantidad .
      ENDIF.
*}   REPLACE
    les_packing-pstyv    = ''.
    les_packing-meins    = i_es_detalle-umc.

*{   DELETE         ER3K900332                                        7
*\  ENDIF.
*}   DELETE

*  Obtener datos del transporte.
*{   REPLACE        ER3K900332                                        6
*\  CALL FUNCTION 'RV_SHIPMENT_VIEW'
*\    EXPORTING
*\      shipment_number       = i_tknum
*\      option_items          = 'X'
*\      option_package_dialog = 'X'
*\      option_partners       = 'X'
*\    EXCEPTIONS
*\      not_found             = 1
*\      no_authority          = 2
*\      delivery_missing      = 3
*\      delivery_lock         = 4
*\      OTHERS                = 5.
  CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number       = i_tknum
      option_items          = 'X'
      option_package_dialog = 'X'
      option_partners       = 'X'
      OPTION_DELIVERY_LOCK  = 'X'
      OPTION_TVTK  = 'X'
      OPTION_TTDS = 'X'
      OPTION_FLOW = 'X'
     IMPORTING

      f_vttkvb         = les_vttkvb
    TABLES
      f_vttp           = lti_xvttp
      f_trlk           = lti_xtrlk
      f_trlp           = lti_xtrlp
    EXCEPTIONS
      not_found             = 1
      no_authority          = 2
      delivery_missing      = 3
      delivery_lock         = 4
      OTHERS                = 5.
*}   REPLACE
  IF sy-subrc NE 0.
*    En caso de alguna excepcion se llena tabla de errores.
*{   REPLACE        ER3K900332                                        9
*\    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-num_doc = i_entrega.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       15
*\    CONCATENATE 'No se pueden obtener datos del transporte' i_tknum
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S022(ZCLPACK) WITH i_tknum into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

**refrech buffer
*  CALL FUNCTION 'HU_INITIALIZE_PACKING'.

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  SELECT SINGLE venum
    FROM vekp
    INTO lv_venum
    WHERE exidv EQ i_es_detalle-umanipulacion.

*  En caso de que la unidad de manipulación exista se mapea,
*  de lo contrario se llena mensajes de error.
  IF sy-subrc EQ 0.

    les_venum-venum = lv_venum.
    APPEND les_venum TO lti_venum.

  ELSE.

*{   REPLACE        ER3K900332                                       12
*\    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-num_doc = i_entrega.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       16
*\    CONCATENATE 'Unidad de manipulación con identificador externo'
*\                i_es_detalle-umanipulacion 'no existe'
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S023(ZCLPACK) WITH i_es_detalle-umanipulacion  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.
*  Mapear datos de items.
  les_packing-venum = lv_venum.
  les_packing-exidv = i_es_detalle-umanipulacion.

* Obtener detalles de unidades de manipulación. Esta funcion
* llena las tablas globales del grupo de funciones.
  CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      if_lock_hus = 'X'
      it_venum    = lti_venum[]
    IMPORTING
      et_header   = lti_header
      et_items    = lti_huitem
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
*    En caso de error se llena tabla con mensajes de  error.
*{   REPLACE        ER3K900332                                       13
*\    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-num_doc = i_entrega.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       17
*\    CONCATENATE 'No se puede obtener datos de unidad de manipulación'
*\                i_es_detalle-umanipulacion
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S024(ZCLPACK) WITH i_es_detalle-umanipulacion  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

*  Funcion para empacar material.
  CALL FUNCTION 'HU_PACKING_AND_UNPACKING'
    EXPORTING
      if_repack          = ' '
      is_packing_request = les_packing
    IMPORTING
      ef_rcode           = lv_subrc
      es_p_request       = les_request
      es_item            = les_item
    CHANGING
      cs_header          = les_header
    EXCEPTIONS
      missing_data       = 1
      hu_not_changeable  = 2
      not_possible       = 3
      customizing        = 4
      weight             = 5
      volume             = 6
      serial_nr          = 7
      fatal_error        = 8
      OTHERS             = 9.
  IF sy-subrc <> 0.
*    En caso de error se llena tabla con mensajes de  error.
*{   REPLACE        ER3K900332                                       14
*\    les_mensajes-num_doc = i_es_detalle-vbeln2.
    les_mensajes-num_doc = i_entrega.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                       18
*\    CONCATENATE 'Error embalando material' i_es_detalle-material
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S025(ZCLPACK) WITH i_es_detalle-material  into les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

*  Verificar que no halla mensajes de error para continuar.
  CHECK c_mensajes IS INITIAL.

  CALL FUNCTION 'HU_POST'.

  COMMIT WORK AND WAIT.

*}   INSERT
  endmethod.


METHOD print_unidades.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Impresión Unidades de Embalaje
* Autor Prog.  :
* Fecha Creac. : 23.10.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*TABLES: nast, itcpo.
*{   INSERT         ER3K900332                                        8
*.... Tipo para datos de entrega
  TYPES: BEGIN OF ltp_likp ,
          VBELN type VBELN_VL,
          vstel type vstel ,
          kunag type kunag ,
          kunnr type KUNWE ,
    END OF ltp_likp.
*.... Tipo para datos de cliente
    TYPES: BEGIN OF ltp_kna1 ,
          kunnr type KUNNR,
          name1 type NAME1_GP,
          stras type STRAS_GP,
          telf1 type TELF1,
          ort01 type ORT01_GP,
    END OF ltp_kna1.
*}   INSERT

  CONSTANTS: dc_x TYPE c VALUE 'X'.
  DATA:  lv_documento TYPE ze_documento.
  DATA: l_funcion              TYPE rs38l_fnam.
  DATA: output_options_smf     TYPE ssfcompop,
        control_parameters_smf TYPE ssfctrlop.
  DATA: lv_cod_barras TYPE zed_cod_barras.
  DATA: wa_zmm_t_imp_etiq TYPE zmm_t_imp_etiq.
  DATA: lv_bl TYPE zed_bl.
  DATA: lv_indicador  TYPE c.
  DATA: lv_confirm_log TYPE zed_flag_confirm.
  DATA: wa_materiales TYPE zmm_str_imprime_etiqueta.
  DATA: lv_confirm_mat TYPE zed_flag_confirm.
  DATA: lv_ean TYPE marm-ean11.
  DATA: lv_unidad_medida(3) TYPE c.
  DATA: wa_t006a    TYPE t006a.
  DATA: local_print_options    TYPE itcpo.
  DATA: local_nast_data        TYPE nast.

*... Estructura para cabecera de entregas
*{   REPLACE        ER3K900332                                        9
*\  DATA: les_likp TYPE likp.
  DATA: les_likp TYPE ltp_likp.
*}   REPLACE
*... Estructura para cabecera de entregas
*{   REPLACE        ER3K900332                                       11
*\  DATA: les_kna1 TYPE kna1.
*\  DATA: les_kna2 TYPE kna1.
  DATA: les_kna1 TYPE ltp_kna1.
  DATA: les_kna2 TYPE ltp_kna1.
*}   REPLACE
*... Estructura para la cabecera de documento
  DATA: les_cab_ref TYPE zesd_cab_ref.
*... Tablas para el manejo del trasporte
  DATA: lti_packing_tr TYPE STANDARD TABLE OF zmm_t_transporte,
        les_packing_tr TYPE zmm_t_transporte.
*.... Estructura para el manejo de mensajes
  DATA: r_mensajes TYPE STANDARD TABLE OF zesd_msg_packing,
        l_mensajes TYPE zesd_msg_packing.

  DATA: l_e_exidv TYPE  exidv,
        l_e_exidv_se TYPE zed_venum,
        l_e_documento TYPE ze_documento,
        l_e_consecutivo TYPE  zed_consecutivo.
*.....Variable  de Ref a la Interfaz
  DATA: go_embalaje TYPE REF TO zcl_lgtica_embalaje,
        go_transporte TYPE REF TO zcl_lgtica_transporte.


*.... Consultamos la impresora asignada al usuario
*{   REPLACE        ER3K900332                                        1
*\  SELECT SINGLE *
  SELECT SINGLE MANDT USUARIO COD_IMPRESORA CENTRO ALMACEN
*}   REPLACE
  INTO wa_zmm_t_imp_etiq
  FROM zmm_t_imp_etiq
  WHERE usuario EQ i_usuario.
*.... si el usuario tiene configurada una impresora
  IF sy-subrc  EQ 0.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname                 = 'ZSD_SMF_PACKING'  "Nombre del formulario
*   VARIANT                  = ' '
*   DIRECT_CALL              = ' '
        IMPORTING
          fm_name                  = l_funcion
        EXCEPTIONS
          no_form                  = 1
          no_function_module       = 2
          OTHERS                   = 3.

    IF sy-subrc <> 0.
*MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    local_print_options-tdcopies   = 1.         " Cantidad de impresiones
    local_print_options-tddest     = wa_zmm_t_imp_etiq-cod_impresora.   " Nombre de la impresora
    local_print_options-tdimmed    = 'X'.       " Imprime inmediatamente
    local_print_options-tdnewid    = 'X'.       " Crear nueva SPOOL
    local_print_options-tddelete   = 'X'.       " Borra después de imprimir
    local_print_options-tdpageslct = space.     " Todas las páginas
    local_print_options-tdpreview  = space.     " Visualización de la impresión
    local_print_options-tdcover    = space.     " No portada

    MOVE-CORRESPONDING local_print_options TO output_options_smf.
    MOVE local_print_options-tdpreview TO control_parameters_smf-preview.
    control_parameters_smf-no_dialog = 'X'.
    control_parameters_smf-langu = local_nast_data-spras.

    READ TABLE i_det_cabecera INTO les_cab_ref INDEX 1.
*{   INSERT         ER3K900332                                        4
    IF les_cab_ref-vbeln is INITIAL.
      les_cab_ref-vbeln = i_entrega.
    ENDIF.
*}   INSERT


    CASE i_tipo_documento.
      WHEN 'ENTR'.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_cab_ref-vbeln
          IMPORTING
            output = l_e_documento.


*... Consulto la información del cliente y destinatario
*{   REPLACE        ER3K900332                                       10
*\        SELECT SINGLE *
*\          INTO les_likp
*\          FROM likp
*\          WHERE vbeln = les_cab_ref-vbeln.
        SELECT SINGLE vbeln vstel kunag kunnr
          INTO les_likp
          FROM likp
          WHERE vbeln = les_cab_ref-vbeln.
*}   REPLACE

*{   REPLACE        ER3K900332                                        3
*\        IF les_likp-vstel IS INITIAL.
*\          les_likp-vstel = '1200'.
*\        ENDIF.
        IF les_likp-vstel IS INITIAL.
          les_likp-vstel = '1200'.
        ELSE.
          CONCATENATE '1' les_likp-vstel+1(3) into les_likp-vstel.
        ENDIF.
*}   REPLACE


*{   REPLACE        ER3K900332                                       12
*\*... Consulto la información del cliente 1
*\        SELECT SINGLE *
*\          INTO les_kna1
*\          FROM kna1
*\          WHERE  kunnr = les_likp-kunag.
*\
*\*... Consulto la información del cliente 1
*\        SELECT SINGLE *
*\          INTO les_kna2
*\          FROM kna1
*\          WHERE  kunnr = les_likp-kunnr.
*... Consulto la información del cliente 1
        SELECT SINGLE kunnr name1 stras telf1 ort01
          INTO les_kna1
          FROM kna1
          WHERE  kunnr = les_likp-kunag.

*... Consulto la información del cliente 1
        SELECT SINGLE kunnr name1 stras telf1 ort01
          INTO les_kna2
          FROM kna1
          WHERE  kunnr = les_likp-kunnr.
*}   REPLACE


**.... Consulto transporte de la entrega
*        SELECT *
*          INTO TABLE lti_packing_tr
*          FROM zmm_t_transporte
*          WHERE documento  = l_e_documento.
*{   REPLACE        ER3K900332                                       15
*\      WHEN 'RINV'.
      WHEN 'RINV' OR 'OPSG'.
*}   REPLACE
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_cab_ref-NUM_DOC
          IMPORTING
            output = l_e_documento.

        CLEAR:les_likp-vstel.

*{   REPLACE        ER3K900332                                       13
*\*... Consulto la información del cliente 1
*\        SELECT SINGLE *
*\          INTO les_kna1
*\          FROM kna1
*\          WHERE  kunnr = les_cab_ref-cod_cliente.
*... Consulto la información del cliente 1
        SELECT SINGLE kunnr name1 stras telf1 ort01
          INTO les_kna1
          FROM kna1
          WHERE  kunnr = les_cab_ref-cod_cliente.
*}   REPLACE

        les_kna2 = les_kna1.
*{   INSERT         ER3K900332                                        5
*... Consulto la información del cliente y destinatario
        SELECT SINGLE vbeln vstel kunag kunnr
          INTO les_likp
          FROM likp
          WHERE vbeln = les_cab_ref-vbeln.

        IF les_likp-vstel IS INITIAL.
          les_likp-vstel = '1200'.
        ELSE.
          CONCATENATE '1' les_likp-vstel+1(3) into les_likp-vstel.
        ENDIF.

*}   INSERT

      WHEN OTHERS.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_cab_ref-NUM_DOC
          IMPORTING
            output = l_e_documento.

*... Consultar Info Cliente
        CLEAR:les_likp-vstel.
        les_kna1-name1 = les_cab_ref-cod_cliente.

        les_kna1 = les_kna2.
*{   INSERT         ER3K900332                                        6
*... Consulto la información del cliente y destinatario
        SELECT SINGLE vbeln vstel kunag kunnr
          INTO les_likp
          FROM likp
          WHERE vbeln = les_cab_ref-vbeln.
        IF les_likp-vstel IS INITIAL.
          les_likp-vstel = '1200'.
        ELSE.
          CONCATENATE '1' les_likp-vstel+1(3) into les_likp-vstel.
        ENDIF.

*}   INSERT
    ENDCASE.

    CALL METHOD zcl_lgtica_transporte=>get_data_by_doc
      EXPORTING
        i_tipo_doc   = i_tipo_documento
        i_documento  = l_e_documento
      IMPORTING
        e_transporte = lti_packing_tr
      CHANGING
        c_mensajes   = c_return.
*
**.... Consulto transporte de la entrega
*    SELECT *
*      INTO TABLE lti_packing_tr
*      FROM zmm_t_transporte
*      WHERE documento  = l_e_documento.
*... Verifico que haya algun registro en la tabla

    IF lti_packing_tr IS NOT INITIAL.
*{   REPLACE        ER3K900332                                       14
*\      READ TABLE lti_packing_tr INTO les_packing_tr WITH KEY documento = l_e_documento.
      READ TABLE lti_packing_tr INTO les_packing_tr WITH KEY documento = l_e_documento vbeln = i_entrega.
      IF sy-subrc ne 0.
        READ TABLE lti_packing_tr INTO les_packing_tr WITH KEY documento = l_e_documento.
        IF sy-subrc EQ 0 .
                CALL METHOD zcl_lgtica_transporte=>asociar_documento
                EXPORTING
                  i_numero_trans    = les_packing_tr-tknum
                  i_numero_trans_se = les_packing_tr-tknum_se
                  i_tipo_doc        = i_tipo_documento
                  i_documento       = l_e_documento
                  i_entrega       = i_entrega
                CHANGING
                  c_return          = r_mensajes.

*        ... Valido que no hayan errores en la creación del transporte
              READ TABLE r_mensajes INTO l_mensajes WITH KEY type_msg = 'E'.
              IF sy-subrc EQ 0 .
                RETURN.
              ENDIF.
        ENDIF.
      ENDIF.
*}   REPLACE
    ELSE.


*... llamo al metodo estatico para crear el trasnporte
      CALL METHOD zcl_lgtica_transporte=>crear_transporte
        EXPORTING
          i_puesto_trab     = les_likp-vstel
          i_clase_trans     = 'ZTR1'
          i_tipo_doc        = i_tipo_documento
          i_usuario         = i_usuario
        IMPORTING
          e_numero_trans    = les_packing_tr-tknum
          e_numero_trans_se = les_packing_tr-tknum_se
        CHANGING
          c_return          = r_mensajes.
*... Valido que no hayan errores en la creación del transporte
      READ TABLE r_mensajes INTO l_mensajes WITH KEY type_msg = 'E'.
      IF sy-subrc EQ 0 .
        RETURN.
      ENDIF.

      CLEAR r_mensajes.
*      MOVE les_cab_ref-vbeln TO lv_documento.
*... llamo al metodo estatico para asociar la entrega al trasnporte
*{   REPLACE        ER3K900332                                        7
*\      CALL METHOD zcl_lgtica_transporte=>asociar_documento
*\        EXPORTING
*\          i_numero_trans    = les_packing_tr-tknum
*\          i_numero_trans_se = les_packing_tr-tknum_se
*\          i_tipo_doc        = i_tipo_documento
*\          i_documento       = l_e_documento
*\        CHANGING
*\          c_return          = r_mensajes.
      CALL METHOD zcl_lgtica_transporte=>asociar_documento
        EXPORTING
          i_numero_trans    = les_packing_tr-tknum
          i_numero_trans_se = les_packing_tr-tknum_se
          i_tipo_doc        = i_tipo_documento
          i_documento       = l_e_documento
          i_entrega       = i_entrega
        CHANGING
          c_return          = r_mensajes.
*}   REPLACE
*... Valido que no hayan errores en la creación del transporte
      READ TABLE r_mensajes INTO l_mensajes WITH KEY type_msg = 'E'.
      IF sy-subrc EQ 0 .
        RETURN.
      ENDIF.
    ENDIF.


*
**... Consulto la información del cliente 1
*    SELECT SINGLE *
*      INTO les_kna1
*      FROM kna1
*      WHERE  kunnr = les_likp-kunag.
*
**... Consulto la información del cliente 1
*    SELECT SINGLE *
*      INTO les_kna2
*      FROM kna1
*      WHERE  kunnr = les_likp-kunnr.


    CALL METHOD zcl_lgtica_embalaje=>crear_um
      EXPORTING
        i_material       = i_material_emb
        i_werks          = les_likp-vstel
        i_tknum          = les_packing_tr-tknum
        i_tknum_se       = les_packing_tr-tknum_se
        i_tipo_documento = i_tipo_documento
        i_usuario        = i_usuario
      IMPORTING
        e_numero_hu      = l_e_exidv
        e_numero_hu_se   = l_e_exidv_se
        e_consecutivo    = l_e_consecutivo
      CHANGING
        c_mensajes       = r_mensajes.



*{   REPLACE        ER3K900332                                        2
*\*         Llamo al smartform
*\    CALL FUNCTION l_funcion
*\      EXPORTING
*\        control_parameters = control_parameters_smf
*\        output_options     = output_options_smf
*\        user_settings      = ' '
*\        nro_transporte     = les_packing_tr-tknum
*\        cliente            = les_kna1-name1
*\        destinatario       = les_kna2-name1
*\        direccion          = les_kna1-stras
*\        telefono1          = les_kna1-telf1
*\        telefono2          = les_kna2-telf1
*\        ciudad             = les_kna2-ort01
*\        entrega            = l_e_documento
*\        consecutivo        = l_e_consecutivo
*\      EXCEPTIONS
*\        formatting_error   = 1
*\        internal_error     = 2
*\        send_error         = 3
*\        user_canceled      = 4
*\        OTHERS             = 5.
*         Llamo al smartform
  CASE i_tipo_documento.
    WHEN 'ENTR' OR 'RINV' OR 'OSIN' OR 'OPSG'.
          CALL FUNCTION l_funcion
      EXPORTING
        control_parameters = control_parameters_smf
        output_options     = output_options_smf
        user_settings      = ' '
        nro_transporte     = les_packing_tr-tknum
        cliente            = les_kna1-name1
        destinatario       = les_kna2-name1
        direccion          = les_kna2-stras
        telefono1          = les_kna1-telf1
        telefono2          = les_kna2-telf1
        ciudad             = les_kna2-ort01
        entrega            = l_e_documento
        consecutivo        = l_e_consecutivo
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    WHEN OTHERS.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input         = les_packing_tr-tknum_se
       IMPORTING
         OUTPUT        = les_packing_tr-tknum
                .


          CALL FUNCTION l_funcion
      EXPORTING
        control_parameters = control_parameters_smf
        output_options     = output_options_smf
        user_settings      = ' '
        nro_transporte     = les_packing_tr-tknum
        cliente            = les_kna1-name1
        destinatario       = les_kna2-name1
        direccion          = les_kna1-stras
        telefono1          = les_kna1-telf1
        telefono2          = les_kna2-telf1
        ciudad             = les_kna2-ort01
        entrega            = l_e_documento
        consecutivo        = l_e_consecutivo
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

  ENDCASE.


*}   REPLACE
*.... Si no hubo errores agrego el transporte y consecutivo a los mensajes
    IF sy-subrc EQ 0.
      CLEAR: l_mensajes.
      l_mensajes-num_doc = l_e_documento.
      CONCATENATE les_packing_tr-tknum les_packing_tr-tknum_se l_e_consecutivo INTO l_mensajes-msg SEPARATED BY space.
      l_mensajes-type_msg = 'I'.
      APPEND l_mensajes TO c_return.
    ENDIF.
  ENDIF.





ENDMETHOD.


METHOD update_ubicacion.

  DATA: les_rotulo TYPE zedsd_rotulo,
        les_transp_dt TYPE zmm_t_transp_dt,
        lti_transp_dt TYPE TABLE OF zmm_t_transp_dt,
        les_mensajes TYPE zesd_msg_packing,
        lv_subrc LIKE sy-subrc.

  les_rotulo = i_rotulo.

*  Obtener datos de detalle de tablas de packing.
*{   REPLACE        ER3K900346                                        1
*\  SELECT SINGLE *
  SELECT SINGLE MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV  CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
*}   REPLACE
    FROM zmm_t_transp_dt
    INTO les_transp_dt
    WHERE tknum EQ les_rotulo-tknum
      AND consecutivo EQ les_rotulo-consecutivo.

  IF sy-subrc EQ 0.
*    Mapeo dato de ubicacion.
    les_transp_dt-ubicacion = i_ubicacion.
    les_transp_dt-estado = ''.
    APPEND les_transp_dt TO lti_transp_dt.
*    Modificar registro de tablas de packing.
    CALL METHOD zcl_lgtica_util=>modify_zmm_t_transp_dt
      EXPORTING
        i_ti_transp_dt  = lti_transp_dt
        i_mod           = 'M'
      IMPORTING
*        e_dbcnt         =
        e_subrc         = lv_subrc.
    IF lv_subrc EQ 0.
      les_mensajes-num_doc = i_rotulo(10).
      les_mensajes-type_msg = 'S'.
*{   REPLACE        ER3K900346                                        2
*\      les_mensajes-msg = 'Se ha actualizado correctamente el registro'.
*      les_mensajes-msg = 'Se ha actualizado correctamente el registro'.
      MESSAGE S029(ZCLPACK) INTO les_mensajes-msg.
*}   REPLACE
      APPEND les_mensajes TO c_mensajes.

    ENDIF.
  ELSE.
*    En caso de que no halla datos en tabla de packing se agrega error.
    les_mensajes-num_doc = i_rotulo(10).
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900346                                        3
*\    les_mensajes-msg = 'Rotulo no existe en tablas de detalle de packing'.
    MESSAGE S030(ZCLPACK) WITH i_rotulo(10) INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.

  ENDIF.

ENDMETHOD.


METHOD VACIAR_UM.

  DATA: les_items TYPE bapihuitmunpack,
        les_vekp  TYPE vekp.

  DATA: lti_header TYPE  hum_hu_header_t,
        lti_huitem TYPE  hum_hu_item_t,
        lv_venum   TYPE  venum,
        lti_venum  TYPE  hum_venum_t,
        les_venum  LIKE LINE OF lti_venum,
        les_mensajes TYPE zesd_msg_packing.

*  Obtenemos el numero de identificacion interno de la unidad
*  de manipulación.
  SELECT SINGLE venum
    FROM vekp
    INTO lv_venum
    WHERE exidv EQ i_exidv.

*  Si se encuentran datos se mapea de l ocontrario se llena error.
  IF sy-subrc EQ 0.
    les_venum-venum = lv_venum.
    APPEND les_venum TO lti_venum.
  ELSE.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900279                                        1
*\    CONCATENATE 'No se encontraron datos de cabecera para unidad de manipulación'
*\       i_exidv INTO les_mensajes-msg SEPARATED BY space.
     MESSAGE S015(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

**get HU details. this will fill the global tables.
  CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      if_lock_hus = 'X'
      it_venum    = lti_venum[]
    IMPORTING
      et_header   = lti_header
      et_items    = lti_huitem
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900279                                        2
*\    CONCATENATE 'Error obteniendo datos de unidad de manipulación' i_exidv
*\      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S016(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

*  Vaciar unidad de manipulación.
  CALL FUNCTION 'HU_EMPTY_HU'
    EXPORTING
      if_exidv      = i_exidv
    EXCEPTIONS
      input_missing = 1
      hu_not_found  = 2
      no_contens    = 3
      not_possible  = 4
      fatal_error   = 5
      OTHERS        = 6.
  IF sy-subrc EQ 3.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'W'.
*{   REPLACE        ER3K900279                                        3
*\    CONCATENATE 'Unidad de manipulación' i_exidv 'no tiene contenido.'
*\      INTO les_mensajes-msg SEPARATED BY space.
*    CONCATENATE 'Unidad de manipulación' i_exidv 'no tiene contenido.'
*      INTO les_mensajes-msg SEPARATED BY space.
      MESSAGE S017(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ELSEIF sy-subrc NE 0.
    les_mensajes-num_doc = i_exidv.
    les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900279                                        4
*\    CONCATENATE 'Error al vaciar unidad de manipulación' i_exidv
*\      INTO les_mensajes-msg SEPARATED BY space.
*    CONCATENATE 'Error al vaciar unidad de manipulación' i_exidv
*      INTO les_mensajes-msg SEPARATED BY space.
    MESSAGE S018(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

  CHECK c_mensajes IS INITIAL.

  CALL FUNCTION 'HU_POST'
    EXPORTING
      if_synchron = 'X'
      if_commit   = 'X'.

  COMMIT WORK AND WAIT.

*  Mensajes
  les_mensajes-num_doc = i_exidv.
  les_mensajes-type_msg = 'S'.
*{   REPLACE        ER3K900279                                        5
*\  CONCATENATE 'Se ha vaciado unidad de manipulación' i_exidv
*\    INTO les_mensajes-msg SEPARATED BY space.
*  CONCATENATE 'Se ha vaciado unidad de manipulación' i_exidv
*    INTO les_mensajes-msg SEPARATED BY space.
  MESSAGE S019(ZCLPACK) WITH i_exidv INTO les_mensajes-msg.
*}   REPLACE
  APPEND les_mensajes TO c_mensajes.

ENDMETHOD.
ENDCLASS.
