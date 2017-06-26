class ZCL_LGTICA_PICKING definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_PICKING
*"* do not include other source files here!!!
public section.

  data PICKNUM type ZED_PICKNUM .
  data S_PICKING type ZEDSD_PICKING .
  data T_PICKING_DET type ZTTSD_PICKING_DET .
  data T_PICKING_SER type ZTTSD_PICKING_SER .
  data T_PICKING_LOT type ZTTSD_PICKING_LOT .

  class-methods SET_CREATE_BY_PKBLE_REF
    importing
      value(I_REF_PICKABLE) type ref to ZIF_LGTICA_PICKABLE
    exporting
      value(E_PICKNUM) type ZTTSD_PICKNUM .
  class-methods CREATE_BY_PKBLE_REF
    importing
      value(I_REF_PICKABLE) type ref to ZIF_LGTICA_PICKABLE
    exporting
      value(E_PICKNUM) type ZTTSD_PICKNUM .
  methods ADD_CANTIDAD
    importing
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO optional
      value(I_ENTREGA) type VBELN_VL optional
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL
    changing
      value(C_CAB_POSICIONES) type ZTTSD_PICKING_DET
      value(C_REG_POSICIONES) type ZTTSD_PICKING_DET
      value(C_MENSAJES) type ZTTSD_MSG_PICKING
      value(C_PICKING_DET_SUP) type ZTTSD_PICKING_DET
      value(C_PICKING_SER) type ZTTSD_PICKING_SER
      value(C_PICKING_LOT) type ZTTSD_PICKING_LOT .
  class-methods GET_DATA_BY_PKBLE_REF
    importing
      value(I_REF_PICKABLE) type ref to ZIF_LGTICA_PICKABLE
      value(I_INDICADOR) type C optional
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL optional
      value(I_FLAG_AGRUP) type ZED_FLAG_AGRUP optional
    changing
      value(C_PICKING_CAB_SUP) type ZTTSD_PICKING
      value(C_PICKING_DET_SUP) type ZTTSD_PICKING_DET_SUP
      value(C_ENTREGA_PICKING) type ZTTSD_ENTREGA_PICKING
      value(C_PICKING_LOT) type ZTTSD_PICKING_LOT optional
      value(C_PICKING_SER) type ZTTSD_PICKING_SER optional .
  methods LOAD_PICKING
    importing
      value(I_PICKNUM) type ZED_PICKNUM .
  methods CERRAR_PICKING
    importing
      value(I_PICKNUM) type ZED_PICKNUM
      value(I_USUARIO) type ZED_USUARIO_MOVIL .
  class-methods GET_DATA_BY_ENTREGA
    importing
      value(I_ENTREGA) type VBELN_VL
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  methods UPDATE_PICKING_BY_CANT
    importing
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO optional
      value(I_ENTREGA) type VBELN_VL optional
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL
    changing
      value(C_CAB_POSICIONES) type ZTTSD_PICKING_DET
      value(C_REG_POSICIONES) type ZTTSD_PICKING_DET
      value(C_MENSAJES) type ZTTSD_MSG_PICKING
      value(C_PICKING_DET_SUP) type ZTTSD_PICKING_DET
      value(C_PICKING_SER) type ZTTSD_PICKING_SER
      value(C_PICKING_LOT) type ZTTSD_PICKING_LOT .
  class-methods GET_DATA_BY_ORDEN
    importing
      value(I_ORDEN) type AUFNR
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  class-methods GET_DATA_BY_RSNUM
    importing
      value(I_RSNUM) type RSNUM
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  class-methods ADD_SER_LOT
    importing
      !I_DOCUMENTO_ORIGEN type ZE_DOCUMENTO
      !I_ENTREGA type VBELN_VL
      !I_TIPO_DOCUMENTO type ZE_TIPO_DOC
      !I_CLASE_DOCUMENTO type ZE_CLAS_DOCUMENTO
      !I_USUARIO type ZED_USUARIO_MOVIL
      !C_MENSAJES type ZTTSD_MSG_PICKING
    changing
      !C_PICKING_SER type ZTTSD_PICKING_SER
      !C_PICKING_LOT type ZTTSD_PICKING_LOT
      !C_REG_POSICIONES type ZTTSD_PICKING_DET optional
      !C_PICKING_DET_SUP type ZTTSD_PICKING_DET optional .
  class-methods GET_DATA_BY_AGLUTINADOR
    importing
      value(I_AGLUTINADOR) type ZED_AGLUTINADOR optional
    returning
      value(R_PICKNUM_AGLUTINADOR) type ZTTSD_CAB_PICKUM_AGLU .
  class-methods GET_DATA_BY_ENTREGA_PND
    importing
      value(I_ENTREGA) type VBELN_VL optional
      value(I_T_ENTREGAS) type ZTTSD_ENT_PKG optional
    changing
      value(E_T_PICKING) type ZTTSD_PICKING optional
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  class-methods MODIFY_PICKING_BY_RECIBO .
  class-methods CREATE_BY_PKBLE_REF_PVTA
    importing
      value(I_REF_PICKABLE) type ref to ZIF_LGTICA_PICKABLE
    exporting
      value(E_PICKNUM) type ZTTSD_PICKNUM .
  class-methods CREATE_BY_PKBLE_REF_PCMP
    importing
      value(I_REF_PICKABLE) type ref to ZIF_LGTICA_PICKABLE
    exporting
      value(E_PICKNUM) type ZTTSD_PICKNUM .
  class-methods GET_DATA_BY_PEDIDO
    importing
      value(I_PEDIDO) type EBELN
    changing
      value(C_PICKING_CAB_SUP) type ZTTSD_PICKING
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  class-methods GET_DATA_BY_ORDEN_AG
    importing
      value(I_ORDEN) type AUFNR
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
  class-methods GET_DATA_BY_ORDEN_B
    importing
      value(I_ORDEN) type AUFNR
    returning
      value(E_PICKNUM) type ZED_PICKNUM .
protected section.
*"* protected components of class ZCL_LGTICA_PICKING
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_PICKING
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_PICKING IMPLEMENTATION.


METHOD add_cantidad.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Distribuye Registros en Tabla de posiciones Picking
* para adiciones
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.... Variables y Estructuras para contabilización
  DATA:
*... Centro
        lv_plant TYPE werks_d,
*.....Mensaje
       l_e_msg TYPE string,
*... Almacen
        lv_stge_loc TYPE lgort_d,
*.... Index para lotes
        l_e_index TYPE sy-tabix,
*.... Material
        l_e_material TYPE matnr,
*.....Seriales Picking
         lti_ser_picking TYPE zttsd_picking_ser,
*.....Estructura para Seriales Picing
         les_ser_picking TYPE zedsd_picking_ser,
*.....Lotes Picking
         lti_lotes_picking TYPE zttsd_picking_lot,
*.....Estructura para Lotes Picking
         les_lotes_picking TYPE zedsd_picking_lot,
*.....Tablas Auxiliares para Tablas y Lotes
         lti_picking_lotes_aux TYPE zttsd_picking_lot,
         lti_picking_ser_aux TYPE zttsd_picking_ser,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.... Estructura de ubicaciones
        les_det_ubicacion TYPE zmm_t_ubicacion,
les_msg_picking TYPE zesd_msg_picking,
*.....Posicion Linea Documento Material
          l_e_linea TYPE mblpo,
*.....Posicion
         l_e_pos TYPE n LENGTH 5,
*.....Control
         l_e_control TYPE i,
*.....Codigo Tx
         l_e_tx TYPE gm_code,
*.....Estructuras para Bapi
         les_header TYPE bapi2017_gm_head_01,                   "Datos de Cabecera
         les_code TYPE bapi2017_gm_code,                        "Codigo Tx
         les_headret TYPE bapi2017_gm_head_ret,                 "Datos de Mov Mercancia
         l_e_matnr TYPE mblnr,                                  "Número Doc. Material
         l_e_year TYPE mjahr,                                   "Ejercicio del Doc. Material
         lti_items TYPE tab_bapi_goodsmvt_item,                 "Posiciones
         lti_serialnumber TYPE tab_bapi_goodsmvt_serialnumber,  "Número Seriales para Posiciones
         les_serialnumber TYPE bapi2017_gm_serialnumber,        "Estructura Seriales
         lti_return TYPE bapiret2_t,                            "Mensajes de Retorno
         les_items TYPE bapi2017_gm_item_create.                "Estructura para Tabla de Pos. Reserv

  DATA: le_flg_aglutinador TYPE c.
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.
*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.
*... Estructura para posición del documento de picking
  DATA: les_picking_dt TYPE zedsd_picking_det.

  DATA: les_cab_posiciones TYPE zedsd_picking_det.
*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_reg_posiciones TYPE zedsd_picking_det.

*.... Posición en el loop
  DATA: lv_index TYPE sy-tabix.

*.... Cantidad diferencia total
  DATA: lv_cantdif_tot TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcontcab_umb TYPE zedsd_cant_cnt.

*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcont_umb TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositio para un material UMD
  DATA: lv_cantcont_umd TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositivo para un material UMB
  DATA: lv_cantdif_umb TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositio para un material UMD
  DATA: lv_cantdif_umd TYPE zedsd_cant_cnt.
  DATA: lv_cons_picknum TYPE zed_picknum.
  DATA: lv_cons_pospick TYPE zed_pospick VALUE 2000.
  DATA: lv_cons_posicion TYPE zed_posicion VALUE 1000.

**... Campos Errores en Procesamiento Cabecera
  DATA: les_msg TYPE zesd_msg_picking.
*.....Variable Texto Error
  DATA:          l_e_desc_error TYPE string.

*.....Variable Texto cantidad excedida Error
  DATA:          l_e_cexc_error TYPE string.
*.... Tablas auxiliares para guardar la información
  DATA: lti_cab_reg_posiciones TYPE zttsd_picking_det,
        lti_reg_posiciones TYPE zttsd_picking_det.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS:
         <lfs_picking_dt> Type  zedsd_picking_det,
         <lfs_detalle> TYPE zesd_eanmat,
         <lfs_return> TYPE bapiret2.

*... Adiciono si el material es con lote y seriales
*  CALL METHOD zcl_lgtica_picking=>add_ser_lot
*    EXPORTING
*      i_documento_origen = i_documento_origen
*      i_entrega          = i_entrega
*      i_tipo_documento   = i_tipo_documento
*      i_clase_documento  = i_clase_documento
*      i_usuario          = i_usuario
*      c_mensajes         = c_mensajes
*    CHANGING
*      c_picking_ser      = c_picking_ser
*      c_picking_lot      = c_picking_lot.



  IF c_picking_det_sup IS NOT INITIAL.
* Obtener centro y almacen segun usuario
    IF i_usuario IS NOT INITIAL.

      select single CENTRO ALMACEN
  into (lv_plant ,lv_stge_loc)
  from zmm_t_imp_etiq
  where usuario eq i_usuario.

    ENDIF.


    SORT c_picking_det_sup DESCENDING BY posicion.
*... obtengo el consecutivo picknum del detalle
    READ TABLE c_picking_det_sup INTO les_reg_posiciones INDEX 1.
    IF sy-subrc EQ 0.
      lv_cons_posicion = lv_cons_posicion + les_reg_posiciones-posicion.
      lv_cons_picknum = les_reg_posiciones-picknum.
    ELSE.
*    lv_cons_picknum = 0.
    ENDIF.
    SORT c_picking_det_sup DESCENDING BY pospick.
*... obtengo el consecutivo pospick del detalle
    READ TABLE c_picking_det_sup INTO les_reg_posiciones INDEX 1.
    IF sy-subrc EQ 0.
      lv_cons_pospick = les_reg_posiciones-pospick.
      lv_cons_picknum = les_reg_posiciones-picknum.
    ELSE.
*    lv_cons_pospick = 0.
    ENDIF.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
      LOOP AT c_reg_posiciones INTO les_reg_posiciones.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_reg_posiciones-material
          IMPORTING
            output = les_mara-matnr.


        APPEND les_mara TO lti_mara.
      ENDLOOP.

*.....Función para Obtener EANs de un Material
      CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
        TABLES
          t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

*.... Recorro los registros enviados desde el dispositivo como adición y los agrego a las tablas de picking
    LOOP AT c_reg_posiciones INTO les_reg_posiciones.
*.... Aumentamos los consecutivos de posicion y posicion de picking
      lv_cons_posicion = lv_cons_posicion + 1.
      lv_cons_pospick = lv_cons_pospick + 1.
*.... Asignamos valores d
*            MOVE-CORRESPONDING les_reg_posiciones to les_picking_dt.
      les_picking_dt-DOC_ASOCIADO = les_reg_posiciones-DOC_ASOCIADO.
      les_picking_dt-material = les_reg_posiciones-material.
      les_picking_dt-picknum = lv_cons_picknum.
      les_picking_dt-cantidad = les_reg_posiciones-cantcont.
      les_picking_dt-umcc  = les_reg_posiciones-umc.
      les_picking_dt-umc  = les_reg_posiciones-umc.
      les_picking_dt-diferencia = 0.
      les_picking_dt-umd  = les_reg_posiciones-umc.
      les_picking_dt-conteo = les_reg_posiciones-conteo.
      les_picking_dt-aufnr = les_reg_posiciones-aufnr.
      les_picking_dt-FECHA = sy-datum.
      les_picking_dt-hora = sy-uzeit.
*            lv_consecutivo = lv_consecutivo + 1.
      les_picking_dt-pospick = lv_cons_pospick.
      les_picking_dt-posicion = lv_cons_posicion.
*            les_picking_dt-cantidad = lv_cantcont_umd.
      les_picking_dt-cantcont = les_reg_posiciones-cantcont.
*            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
      les_picking_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
      les_picking_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
      les_picking_dt-usuario = i_usuario.
*            les_picking_dt-p_confirm = 'OK'.
      les_picking_dt-p_confirm = 'CT'.
      APPEND les_picking_dt TO lti_picking_dt.

*.... Proceso de contabilización de Adiciones
*.... Asigna el Cod. de Tx, y valores a la estructura
      l_e_tx = '03'.
*.....Datos de Cabecera para la Contabilización de Ordenes y Reservas
      les_header-pstng_date = sy-datum.
      les_header-doc_date = sy-datum.
      les_header-pr_uname = i_usuario.
      les_code-gm_code = l_e_tx.

********************
*.... Convertir material
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = les_reg_posiciones-material
        IMPORTING
          output = l_e_material.
*          Error en la asignación de seriales l_e_linea = l_e_linea + 1 .
      DESCRIBE TABLE lti_items LINES l_e_linea.
      l_e_linea = l_e_linea + 1.
*.....Recorro Tabla de Lotes en caso que Exista Posiciones con Lotes
      LOOP AT c_picking_lot INTO les_lotes_picking
                                WHERE matnr = l_e_material AND
                                     ( aufnr = les_reg_posiciones-aufnr OR
                                       rsnum = les_reg_posiciones-rsnum ) AND cantidad_uso > 0.
        l_e_index = sy-tabix.
        l_e_control = 1.
*.....Material del Lote
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_reg_posiciones-material
          IMPORTING
            output = les_items-material.

*            les_items-material = <lfs_detalle_reservas>-material.
*.....Centro Lote
        les_items-plant = lv_plant.
*.....Almacen Lote
        les_items-stge_loc = lv_stge_loc.
*.....Información de Lote
        les_items-batch = les_lotes_picking-lote.
*.....Cantidad Contada para el Lote
        les_items-entry_qnt = les_lotes_picking-cantidad_uso.

        les_items-WITHDRAWN = 'X'.
*.....Número de Orden
        les_items-orderid = les_reg_posiciones-aufnr.

*.... Consulto el registro de la z de ubicaciones
*{   REPLACE        ER3K900309                                        2
*\            SELECT SINGLE *
            SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                          NUMERO_DOC USUARIO  FECHA HORA
*}   REPLACE
              INTO les_det_ubicacion
              FROM zmm_t_ubicacion
               WHERE material EQ les_items-material AND
                centro EQ les_items-plant AND
                almacen EQ les_items-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                ubicacion EQ les_lotes_picking-ubicacion_fi.
*.... Si hay registro descuento la cantidad consumida
            IF sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                EXPORTING
                  matnr      = les_items-material
                TABLES
                  t_detalle  = lti_detalle
                  t_mensajes = lti_mensajes.

              READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_lotes_picking-umc.
              IF sy-subrc EQ 0 .
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
              ELSE.
                CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                  EXPORTING
                    input                = les_lotes_picking-umc
                   language             = sy-langu
                 IMPORTING

                   output               = les_lotes_picking-umc

                          .
                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_lotes_picking-umc.
                IF sy-subrc EQ 0 .
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ENDIF.
              ENDIF.

            ENDIF.

*.... Tipo de movimiento
          les_items-move_type = '261'.

        les_lotes_picking-cantidad_uso = 0.
*            les_lotes_picking-cantidad_uso = les_lotes_picking-cantidad_uso + les_items-entry_qnt.
        MODIFY lti_lotes_picking FROM les_lotes_picking INDEX l_e_index.
        APPEND les_items TO lti_items.
        CLEAR: les_items, les_lotes_picking.
      ENDLOOP.

      IF l_e_control IS INITIAL .


*.....Cantidad
          les_items-entry_qnt = les_reg_posiciones-cantcont . "les_det_picking-cantcont.
*.....Material
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = les_reg_posiciones-material
            IMPORTING
              output = les_items-material.

*              les_items-material = <lfs_detalle_reservas>-material.
*.....Centro
          les_items-plant = lv_plant.
*.....Almacen
          les_items-stge_loc = lv_stge_loc.
**.....Lote
*          les_items-batch = <lfs_detalle_reservas>-batch.
*.....Número de Orden
          les_items-orderid = les_reg_posiciones-aufnr.
          les_items-WITHDRAWN = 'X'.

*.... Consulto el registro de la z de ubicaciones
*{   REPLACE        ER3K900309                                        1
*\              SELECT SINGLE *
              SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                            NUMERO_DOC USUARIO  FECHA HORA
*}   REPLACE
                INTO les_det_ubicacion
                FROM zmm_t_ubicacion
                 WHERE material EQ les_picking_dt-material AND
                  centro EQ les_items-plant AND
                  almacen EQ les_items-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                  ubicacion EQ les_picking_dt-ubicacion_fi.
*.... Si hay registro descuento la cantidad consumida
              IF sy-subrc EQ 0.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = les_picking_dt-material
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.

                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_dt-umc.
                IF sy-subrc EQ 0 .
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_picking_dt-cantcont * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_det_picking-cantcont * <lfs_detalle>-cantidad_ean ).
                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ENDIF.

              ENDIF.
*.....Tipo de Movimiento
            les_items-move_type = '261'.
          APPEND les_items TO lti_items.
*.....Busqueda de Material Serializado
          LOOP AT lti_ser_picking INTO les_ser_picking
                                   WHERE matnr = l_e_material AND
                                         ( aufnr = les_reg_posiciones-aufnr OR
                                       rsnum = les_reg_posiciones-rsnum ) AND flag_ser NE 'X'.

            les_serialnumber-matdoc_itm =  l_e_linea .
            les_serialnumber-serialno = les_ser_picking-serie.
            APPEND les_serialnumber TO lti_serialnumber.

            les_ser_picking-flag_ser = 'X'.

            APPEND les_ser_picking TO lti_picking_ser_aux.
*.....Elimino Material Serializado de tabla de Seriales
*                DELETE lti_ser_picking WHERE matnr = l_e_material AND
*                                             serie = les_ser_picking-serie.
            CLEAR: les_serialnumber, les_ser_picking.
          ENDLOOP.

      ENDIF.
      CLEAR : les_items,  l_e_pos, l_e_control.
********************

    ENDLOOP.
* .....
*.....Bapi encargada de Realizar Contabilizar Documentos Que no Generan Entrega
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header       = les_header
            goodsmvt_code         = les_code
            testrun               = ' '
          IMPORTING
            goodsmvt_headret      = les_headret
            materialdocument      = l_e_matnr
            matdocumentyear       = l_e_year
          TABLES
            goodsmvt_item         = lti_items
            goodsmvt_serialnumber = lti_serialnumber
            return                = lti_return.

        les_msg_picking-num_doc = les_reg_posiciones-aufnr.
*.....Existen Errores a Contabilizar la Orden
        IF lti_return[] IS NOT INITIAL.

          LOOP AT  lti_return ASSIGNING <lfs_return>.
            les_msg_picking-msg = <lfs_return>-message.
            les_msg_picking-type_msg = <lfs_return>-type.
            APPEND les_msg_picking TO c_mensajes.
            CLEAR : les_msg_picking.
          ENDLOOP.
*.....Deshacer Cambios realiazados en Bases de Datos
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE.
*.... Concate en el documento asociado el número único enviado por SUP y el documento de material
          LOOP AT lti_picking_dt ASSIGNING <lfs_picking_dt>.

           CONCATENATE <lfs_picking_dt>-DOC_ASOCIADO '-' l_e_matnr
           INTO <lfs_picking_dt>-DOC_ASOCIADO RESPECTING BLANKS.

          ENDLOOP.

*    ....   Modifico la tabla Z adicionando cantidades
              MODIFY  zmm_t_picking_dt FROM TABLE lti_picking_dt.


        CONCATENATE 'Proceso Exitoso, Número de Documento Contabilización' space l_e_matnr space
           INTO l_e_msg RESPECTING BLANKS.
          les_msg_picking-msg = l_e_msg.
          les_msg_picking-type_msg = 'S'.
          APPEND les_msg_picking TO c_mensajes.

        ENDIF.

  ENDIF.




ENDMETHOD.


METHOD add_ser_lot.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Registros en Tabla de lotes y seriales Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.
*... Estructuras para el manejo de las tablas de Seriales y Lotes
  DATA: les_picking_ser TYPE zedsd_picking_ser,
        les_picking_lot TYPE zedsd_picking_lot.
*{   INSERT         ER3K900279                                        4
  DATA c_picking_lot_x TYPE ZTTSD_PICKING_LOT.

  DATA: lv_tabix type sy-tabix.
  DATA:  les_REG_POSICIONES like LINE OF C_REG_POSICIONES,
         les_PICKING_DET_SUP like LINE OF C_PICKING_DET_SUP.
*}   INSERT
*... Estructuras para el manejo de las tablas de Seriales y Lotes
  DATA: lti_picking_ser TYPE STANDARD TABLE OF zmm_t_pck_ser,
        lti_picking_lot TYPE STANDARD TABLE OF zmm_t_pck_lot.
*... Variables para setear el valor dependiendo de del tipo de documento
  DATA: l_e_documento_ser TYPE zed_documento,
        l_e_documento_lot TYPE zed_documento,
        l_e_vbeln TYPE  vbeln_va,
        l_e_ebeln TYPE  ebeln ,
        l_e_aglutinador TYPE  zed_aglutinador ,
        l_e_vbeln2  TYPE  vbeln_vl  ,
        l_e_aufnr TYPE  aufnr ,
        l_e_rsnum TYPE  rsnum .

*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcontcab_umb TYPE zedsd_cant_cnt.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS: <fs_pck_ser> TYPE zmm_t_pck_ser,
                 <fs_pck_lot> TYPE zmm_t_pck_lot.

*...Valido que hayan ingresado entrega
  IF i_entrega IS NOT INITIAL.
    l_e_vbeln2 = i_entrega.
  ENDIF.
*... Consulto el primer registro de seriales y guardo el documento asociado
  READ TABLE c_picking_ser INTO les_picking_ser INDEX 1.
  IF sy-subrc EQ 0 .
    l_e_documento_ser = les_picking_ser-documento.
  ENDIF.
*... Consulto el primer registro de lotes y guardo el documento asociado
  READ TABLE c_picking_lot INTO les_picking_lot INDEX 1.
  IF sy-subrc EQ 0 .
    l_e_documento_lot = les_picking_lot-documento.
  ENDIF.
  CASE i_tipo_documento.
*Ventas
    WHEN 'PVTA'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_vbeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_vbeln = l_e_documento_lot.
      ENDIF.
    WHEN 'PROY'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_vbeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_vbeln = l_e_documento_lot.
      ENDIF.
*            Compras
    WHEN 'TRAS'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'SUBC'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'DEVO'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'ICOM'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'OPAG'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aglutinador = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aglutinador = l_e_documento_lot.
      ENDIF.
    WHEN 'OPSG'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'OSEX'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'RINV'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_rsnum = l_e_documento_ser.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_rsnum = l_e_documento_lot.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'OSIN'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'OSPR'.

  ENDCASE.

*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_vbeln
    IMPORTING
      output = l_e_vbeln.

*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_ebeln
    IMPORTING
      output = l_e_ebeln.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_aglutinador
    IMPORTING
      output = l_e_aglutinador.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_vbeln2
    IMPORTING
      output = l_e_vbeln2.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_aufnr
    IMPORTING
      output = l_e_aufnr.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_rsnum
    IMPORTING
      output = l_e_rsnum.

*... Inserción de seriales y lotes.
  LOOP AT c_picking_ser ASSIGNING <fs_pck_ser>.
    IF l_e_documento_ser IS NOT INITIAL.
      <fs_pck_ser>-documento = l_e_documento_ser.
    ENDIF.
    <fs_pck_ser>-vbeln = l_e_vbeln .
    <fs_pck_ser>-ebeln = l_e_ebeln.
    <fs_pck_ser>-aglutinador = l_e_aglutinador.
    <fs_pck_ser>-vbeln2 = l_e_vbeln2.
    <fs_pck_ser>-aufnr = l_e_aufnr.
    <fs_pck_ser>-rsnum = l_e_rsnum.
    <fs_pck_ser>-tipo_doc = i_tipo_documento.
  ENDLOOP.
*... Valido que se hayan contado Lotes
  IF c_picking_lot[] IS NOT INITIAL.
*... Consulto si los lotes contados se encuentran en BD
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lti_picking_lot
      FROM zmm_t_pck_lot
      FOR ALL ENTRIES IN c_picking_lot
      WHERE documento EQ c_picking_lot-documento AND
      tipo_doc EQ c_picking_lot-tipo_doc AND
      matnr EQ c_picking_lot-matnr AND
      lote EQ c_picking_lot-lote AND
      ubicacion_fi EQ c_picking_lot-ubicacion_fi.
  ENDIF.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*Modificado por: Andrés Felipe Castro.
  LOOP AT c_picking_lot ASSIGNING <fs_pck_lot>.
    les_mara-matnr = <fs_pck_lot>-matnr.
    APPEND les_mara TO lti_mara.
  ENDLOOP.

*.....Función para Obtener EANs de un Material
  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
    TABLES
      t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

  LOOP AT c_picking_lot ASSIGNING <fs_pck_lot>.
*{   INSERT         ER3K900279                                        3
*    lv_tabix = sy-tabix.
*    READ TABLE C_PICKING_DET_SUP into LES_PICKING_DET_SUP WITH KEY MATERIAL = <fs_pck_lot>-MATNR P_CONFIRM = 'OK'.
*    IF sy-subrc ne 0.
*    READ TABLE C_PICKING_DET_SUP into LES_PICKING_DET_SUP WITH KEY MATERIAL = <fs_pck_lot>-MATNR P_CONFIRM = 'NE' Conteo = '3'.
*    IF sy-subrc ne 0.
*    READ TABLE C_PICKING_DET_SUP into LES_PICKING_DET_SUP WITH KEY MATERIAL = <fs_pck_lot>-MATNR P_CONFIRM = 'NE' conteo = '0'.
*    IF sy-subrc ne 0.
**       UNASSIGN <fs_pck_lot>.
*       DELETE c_picking_lot INDEX lv_tabix.
*       CONTINUE.
*    ELSE.
*      READ TABLE C_PICKING_DET_SUP into LES_PICKING_DET_SUP WITH KEY MATERIAL = <fs_pck_lot>-MATNR P_CONFIRM = 'NE' conteo = '1'.
*      IF sy-subrc eq 0.
**       UNASSIGN <fs_pck_lot>.
*       DELETE c_picking_lot INDEX lv_tabix.
*       CONTINUE.
*      ELSE.
*              READ TABLE C_PICKING_DET_SUP into LES_PICKING_DET_SUP WITH KEY MATERIAL = <fs_pck_lot>-MATNR P_CONFIRM = 'NE' conteo = '2'.
*              IF sy-subrc eq 0.
**                UNASSIGN <fs_pck_lot>.
*                DELETE c_picking_lot INDEX lv_tabix.
*                CONTINUE.
*              ENDIF.
*      ENDIF.
*    ENDIf.
*    ENDIF.
*    ENDIF.
*

*}   INSERT
*... Asigno el documento
    IF l_e_documento_lot IS NOT INITIAL.
      <fs_pck_lot>-documento = l_e_documento_lot.
    ENDIF.

    CLEAR:t_detalle.
*... Consulto los EANS de el material
    CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
       EXPORTING
         matnr            = <fs_pck_lot>-matnr
        TABLES
*       T_MARA           =
          t_detalle        = t_detalle
          t_mensajes       = t_mensajes
                .
    lv_cantcontcab_umb = <fs_pck_lot>-cantidad.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
    READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = <fs_pck_lot>-umc.
    IF sy-subrc EQ 0.
      lv_cantcontcab_umb = <fs_pck_lot>-cantidad /  les_detalle-cantidad_ean.
    ENDIF.
*... Leo si existe el lote en BD
*{   REPLACE        ER3K900279                                        1
*\    READ TABLE lti_picking_lot INTO les_picking_lot WITH KEY documento = <fs_pck_lot>-documento
*\    tipo_doc = <fs_pck_lot>-tipo_doc matnr = <fs_pck_lot>-matnr lote = <fs_pck_lot>-lote.
    READ TABLE lti_picking_lot INTO les_picking_lot WITH KEY documento = <fs_pck_lot>-documento
    tipo_doc = <fs_pck_lot>-tipo_doc matnr = <fs_pck_lot>-matnr lote = <fs_pck_lot>-lote ubicacion_fi = <fs_pck_lot>-UBICACION_FI.
*}   REPLACE
*... Acumulo la cantidad
    IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900279                                        2
*\      <fs_pck_lot>-cantidad_uso = lv_cantcontcab_umb + <fs_pck_lot>-cantidad_uso.
*\      lv_cantcontcab_umb = lv_cantcontcab_umb + les_picking_lot-cantidad.
      <fs_pck_lot>-cantidad_uso = les_picking_lot-CANTIDAD_USO + <fs_pck_lot>-cantidad.
      lv_cantcontcab_umb = lv_cantcontcab_umb + les_picking_lot-cantidad.
*}   REPLACE
    ELSE.
      <fs_pck_lot>-cantidad_uso = lv_cantcontcab_umb.
    ENDIF.

    <fs_pck_lot>-cantidad = lv_cantcontcab_umb.

    <fs_pck_lot>-umc_uso = <fs_pck_lot>-umc_uso.
    <fs_pck_lot>-tipo_doc = i_tipo_documento.
    <fs_pck_lot>-vbeln = l_e_vbeln .
    <fs_pck_lot>-ebeln = l_e_ebeln.
    <fs_pck_lot>-aglutinador = l_e_aglutinador.
    <fs_pck_lot>-vbeln2 = l_e_vbeln2.
    <fs_pck_lot>-aufnr = l_e_aufnr.
    <fs_pck_lot>-rsnum = l_e_rsnum.
*{   INSERT         ER3K900279                                        6
    append <fs_pck_lot> to c_picking_lot_x.
*}   INSERT
  ENDLOOP.


*{   REPLACE        ER3K900279                                        5
*\  MODIFY  zmm_t_pck_ser FROM TABLE c_picking_ser.
*\  MODIFY  zmm_t_pck_lot FROM TABLE c_picking_lot.
  IF c_picking_ser is not INITIAL.
    MODIFY  zmm_t_pck_ser FROM TABLE c_picking_ser.
  ENDIF.
  IF c_picking_lot_x is not INITIAL.
    MODIFY  zmm_t_pck_lot FROM TABLE c_picking_lot_x.
  ENDIF.

*}   REPLACE
ENDMETHOD.


method CERRAR_PICKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Carga los datos de un documento picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tabla interna para la cabecera del documento de picking
  data: lti_picking type standard table of zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  data: les_picking type zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  data: lti_picking_dt type standard table of zmm_t_picking_dt.
*... Estructura interna para la posicion del documento de picking
  data: les_picking_dt type  zmm_t_picking_dt.
*... Variable para llevar el consecutivo de las posiciones del documento de picking
  data: lv_consecutivo type zed_pospick.
*... Valido que se haya ingresado un numero de documento de picking
  if i_picknum is not initial.
*.... Consulto la cabecera para los documento de picking
*{   REPLACE        ER3K900279                                        1
*\      select *  from zmm_t_picking
*\        into table lti_picking
*\        where picknum eq i_picknum.
      select MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
             USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO  from zmm_t_picking
        into table lti_picking
        where picknum eq i_picknum.
*}   REPLACE
*... valido que hayan encontrado registros de cabecera
        if lti_picking[] is not initial.
*... cargo en la estructura el registro encontrado de la cabecera del documento de picking
            read table lti_picking into les_picking index 1.
            if sy-subrc eq 0.
              les_picking-estado = 'CERRADO'.
              les_picking-FEMOD  = sy-datum.
              les_picking-HOMOD  = sy-uzeit.
              les_picking-USRMOD = i_usuario.
              les_picking-UNAMOD = sy-uname.

*...  Actualizo el registro de cabecera
              modify  zmm_t_picking from les_picking .
*... Realizo set del atributo de cabecera del documento de picking
              s_picking = les_picking.

            endif.
*... Consulto el detalle para los documento de picking encontrado
*{   REPLACE        ER3K900279                                        2
*\          select  * from zmm_t_picking_dt
*\            into table lti_picking_dt
*\            for all entries in lti_picking
*\            where picknum eq lti_picking-picknum.
          select  MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
                  UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
                  AUFNR USUARIO
            from zmm_t_picking_dt
            into table lti_picking_dt
            for all entries in lti_picking
            where picknum eq lti_picking-picknum.
*}   REPLACE
*... valido que hayan encontrado registros de detalle del documento de picking
            if lti_picking_dt[] is not initial.

*... Depuro los registros que no se les ha realizado algun conteo
              DELETE lti_picking_dt where CANTCONT = 0.
*{   INSERT         ER3K900279                                        3
              IF lti_picking_dt[] is INITIAL.
*... Borro todos los registros de la tabla
              DELETE from zmm_t_picking where picknum = les_picking-picknum .
              ENDIF.
*}   INSERT

              clear:lv_consecutivo.

              LOOP AT lti_picking_dt into les_picking_dt.
*{   DELETE         ER3K900279                                        5
*\                lv_consecutivo = lv_consecutivo + 1.
*\                les_picking_dt-pospick = lv_consecutivo.
*}   DELETE
                les_picking_dt-cantidad = les_picking_dt-cantcont.
                les_picking_dt-p_confirm = 'CT'.
                modify lti_picking_dt from les_picking_dt index sy-tabix.
              ENDLOOP.
*... Borro todos los registros de la tabla
              DELETE from zmm_t_picking_dt where picknum = les_picking-picknum .
*... Actualizo la tabla Z de detalle de picking con los registros que fueron filtrados
                modify zmm_t_picking_dt from table lti_picking_dt.
*... Realizo set del atributo de detalle del documento de picking
              t_picking_det = lti_picking_dt.
*... Consulto los seriales para los detalles del documento de picking
*{   INSERT         ER3K900279                                        4
             ELSE.
*... Borro todos los registros de la tabla
              DELETE from zmm_t_picking where picknum = les_picking-picknum .

*}   INSERT
            endif.
        endif.
  endif.

endmethod.


METHOD create_by_pkble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.


*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: lti_picking_aux_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: les_picking_aux_dt TYPE zmm_t_picking_dt.

*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_pos_picking TYPE zedsd_picking_det.



DATA: lv_consecutivo type ZED_POSPICK.

   "Declarar variables
  data: wobjeto      type tnro-object    value 'ZEDPICKING', " Nombre del SNRO
        wnorange     type inri-nrrangenr value '01',        "Número de rango,
        wsubobj      type inri-subobject value space,       "Subobject
        w_doc_number type char10.

  data:  p_doc_number type char18. " Variable para almacenar el número generado


*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de picking
  CALL METHOD i_ref_pickable->get_header_picking
    RECEIVING
      r_header_picking = r_header_picking.

*... Válido que existan registros
  IF r_header_picking[] IS NOT INITIAL.

*{   REPLACE        ER3K900279                                        1
*\*... Consulto todos los registros que se hayan creado con anterioridad
*\    SELECT *
*\      FROM zmm_t_picking
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_picking
*\      FOR ALL ENTRIES IN  r_header_picking
*\      WHERE vbeln2 EQ r_header_picking-vbeln2
*\      and vbeln EQ r_header_picking-vbeln
*\      and ebeln EQ r_header_picking-ebeln
*\      and aglutinador EQ r_header_picking-aglutinador
*\      and aufnr EQ r_header_picking-aufnr
*\      and rsnum EQ r_header_picking-rsnum.
*... Consulto todos los registros que se hayan creado con anterioridad
    SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
           USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_picking
      INTO TABLE lti_picking
      FOR ALL ENTRIES IN  r_header_picking
      WHERE vbeln2 EQ r_header_picking-vbeln2
      and vbeln EQ r_header_picking-vbeln
      and ebeln EQ r_header_picking-ebeln
      and aglutinador EQ r_header_picking-aglutinador
      and aufnr EQ r_header_picking-aufnr
      and rsnum EQ r_header_picking-rsnum.
*}   REPLACE
*... Llamo el metodo para Retornara la Estructura de detalle para un document de picking
    CALL METHOD i_ref_pickable->get_pos_picking
      RECEIVING
        r_pos_picking = r_pos_picking.

     IF lti_picking is not initial.
*       Consulto los detalles del picking
*{   REPLACE        ER3K900279                                        2
*\      SELECT *
*\      FROM zmm_t_picking_dt
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_picking_aux_dt
*\      FOR ALL ENTRIES IN  lti_picking
*\      WHERE picknum eq lti_picking-picknum.
      SELECT MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
             UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO
      FROM zmm_t_picking_dt
      INTO  TABLE lti_picking_aux_dt
      FOR ALL ENTRIES IN  lti_picking
      WHERE picknum eq lti_picking-picknum.
*}   REPLACE

       "Filtro los documentos de picking cerrados
       LOOP AT lti_picking into les_picking.
         IF les_picking-estado = 'CERRADO' and ( les_picking-aufnr is initial and les_picking-rsnum is initial ).
*... se borra registro cerrado de la cabecera
                    DELETE r_header_picking where vbeln2 = les_picking-vbeln2 and
                                       vbeln  = les_picking-vbeln and
                                       ebeln  = les_picking-ebeln and
                                       aglutinador = les_picking-aglutinador and
                                       aufnr = les_picking-aufnr and
                                       rsnum = les_picking-rsnum.
*... Se borran los registros de posición del documento cerrado
                    DELETE r_pos_picking where picknum = les_picking-picknum .

         ENDIF.
       ENDLOOP.

     ENDIF.

*... Recorro las cabeceras de picking
    LOOP AT r_header_picking INTO les_header_picking.
      CLEAR:    lv_consecutivo ,les_picking.

*.... Busco si hay algun registro en la cabecera

     READ TABLE lti_picking into les_picking WITH KEY vbeln2 = les_header_picking-vbeln2 vbeln = les_header_picking-vbeln ebeln = les_header_picking-ebeln
      aglutinador = les_header_picking-aglutinador aufnr = les_header_picking-aufnr rsnum = les_header_picking-rsnum.
      IF sy-subrc EQ 0 .

        MOVE les_picking TO les_header_picking.
        IF les_picking-aufnr is initial and les_picking-rsnum is initial.
*... Se borran los registros de posición del documento cerrado
        DELETE from zmm_T_picking_dt where picknum = les_header_picking-picknum .
        ELSE.
*... Asigno estado pendiente otra vez
          les_header_picking-estado = 'PENDIENTE'.
          Select single max( POSPICK )
            from zmm_T_picking_dt
            into lv_consecutivo
            where picknum = les_header_picking-picknum.
        ENDIF.

      ELSE.
      call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = wnorange
        object                  = wobjeto
        subobject               = wsubobj
      importing
        number                  = p_doc_number " Número generado por SAP
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        internal_overflow       = 6
        others                  = 7.

        les_header_picking-picknum = p_doc_number.
        les_header_picking-estado = 'PENDIENTE'.

        ENDIF.

      LOOP AT r_pos_picking into les_pos_picking where vbeln2 = les_header_picking-vbeln2 and RSNUM = les_header_picking-rsnum and CANTIDAD > 0.
          IF les_header_picking-aglutinador is not initial.
            CLEAR:les_picking_aux_dt.
*            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
*            rsnum = les_pos_picking-rsnum.
*... Se añade que la cantidad debe ser igual
            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
            rsnum = les_pos_picking-rsnum cantidad = les_pos_picking-cantidad.
            IF sy-subrc eq 0 .
              CONTINUE.
*            ELSE.
**... Se consulta la cantidad parcial
*            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
*            rsnum = les_pos_picking-rsnum.
            ENDIF.
          ENDIF.
          lv_consecutivo = lv_consecutivo + 1.

*          INICIO SO-133 LFY 14/04/2015.

*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            EXPORTING
*              input         =           les_pos_picking-material
*           IMPORTING
*             OUTPUT        =           les_pos_picking-material

*          FIN SO-133 LFY 14/04/2015.
                    .
*AD
*          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          IF les_pos_picking-cantidad  <= 0 .
              CONTINUE.
          ENDIF.
*AD
          les_pos_picking-picknum = les_header_picking-picknum.
          les_pos_picking-POSPICK = lv_consecutivo.
*MD          les_pos_picking-diferencia = les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-diferencia = les_pos_picking-cantidad.
          les_pos_picking-CANTCONT = 0.
          APPEND les_pos_picking to lti_picking_dt.

      ENDLOOP.

      MODIFY zmm_t_picking from les_header_picking.
      MODIFY zmm_T_picking_dt from TABLE lti_picking_dt.
    ENDLOOP.

  ENDIF.
ENDMETHOD.


METHOD CREATE_BY_PKBLE_REF_PCMP.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.


*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: lti_picking_aux_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: les_picking_aux_dt TYPE zmm_t_picking_dt.

*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_pos_picking TYPE zedsd_picking_det.



DATA: lv_consecutivo type ZED_POSPICK.

   "Declarar variables
  data: wobjeto      type tnro-object    value 'ZEDPICKING', " Nombre del SNRO
        wnorange     type inri-nrrangenr value '01',        "Número de rango,
        wsubobj      type inri-subobject value space,       "Subobject
        w_doc_number type char10.

  data:  p_doc_number type char18. " Variable para almacenar el número generado


*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de picking
  CALL METHOD i_ref_pickable->get_header_picking
    RECEIVING
      r_header_picking = r_header_picking.

*... Válido que existan registros
  IF r_header_picking[] IS NOT INITIAL.

*... Consulto todos los registros que se hayan creado con anterioridad
*{   REPLACE        ER3K900279                                        1
*\    SELECT *
*\      FROM zmm_t_picking
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_picking
*\      FOR ALL ENTRIES IN  r_header_picking
*\      WHERE vbeln2 EQ r_header_picking-vbeln2
*\      and vbeln EQ r_header_picking-vbeln
*\      and ebeln EQ r_header_picking-ebeln
*\      and aglutinador EQ r_header_picking-aglutinador
*\      and aufnr EQ r_header_picking-aufnr
*\      and rsnum EQ r_header_picking-rsnum.
*... Consulto todos los registros que se hayan creado con anterioridad
    SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO
           UNAME FEMOD HOMOD
      FROM zmm_t_picking
      INTO  TABLE lti_picking
      FOR ALL ENTRIES IN  r_header_picking
      WHERE ebeln EQ r_header_picking-ebeln.

*}   REPLACE
*... Llamo el metodo para Retornara la Estructura de detalle para un document de picking
    CALL METHOD i_ref_pickable->get_pos_picking
      RECEIVING
        r_pos_picking = r_pos_picking.

     IF lti_picking is not initial.
*       Consulto los detalles del picking
*{   REPLACE        ER3K900279                                        5
*\      SELECT *
*\      FROM zmm_t_picking_dt
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_picking_aux_dt
*\      FOR ALL ENTRIES IN  lti_picking
*\      WHERE picknum eq lti_picking-picknum.
      SELECT MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
             UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
             AUFNR USUARIO
      FROM zmm_t_picking_dt
      INTO  TABLE lti_picking_aux_dt
      FOR ALL ENTRIES IN  lti_picking
      WHERE picknum eq lti_picking-picknum.
*}   REPLACE

       "Filtro los documentos de picking cerrados
       LOOP AT lti_picking into les_picking.
*{   REPLACE        ER3K900279                                        2
*\         IF les_picking-estado = 'CERRADO' and ( les_picking-aufnr is initial and les_picking-rsnum is initial ).
         IF les_picking-estado = 'CERRADO' .
*}   REPLACE
*... se borra registro cerrado de la cabecera
*{   REPLACE        ER3K900279                                        3
*\                    DELETE r_header_picking where vbeln2 = les_picking-vbeln2 and
*\                                       vbeln  = les_picking-vbeln and
*\                                       ebeln  = les_picking-ebeln and
*\                                       aglutinador = les_picking-aglutinador and
*\                                       aufnr = les_picking-aufnr and
*\                                       rsnum = les_picking-rsnum.
                    DELETE r_header_picking where vbeln2 = les_picking-vbeln2 and
                                       ebeln  = les_picking-ebeln.

*}   REPLACE
*... Se borran los registros de posición del documento cerrado
                    DELETE r_pos_picking where picknum = les_picking-picknum .

         ENDIF.
       ENDLOOP.

     ENDIF.

*... Recorro las cabeceras de picking
    LOOP AT r_header_picking INTO les_header_picking.
      CLEAR:    lv_consecutivo ,les_picking.

*.... Busco si hay algun registro en la cabecera

*{   REPLACE        ER3K900279                                        4
*\     READ TABLE lti_picking into les_picking WITH KEY vbeln2 = les_header_picking-vbeln2 vbeln = les_header_picking-vbeln ebeln = les_header_picking-ebeln
*\      aglutinador = les_header_picking-aglutinador aufnr = les_header_picking-aufnr rsnum = les_header_picking-rsnum.
     READ TABLE lti_picking into les_picking WITH KEY vbeln2 = les_header_picking-vbeln2 ebeln = les_header_picking-ebeln.
*}   REPLACE
      IF sy-subrc EQ 0 .

        MOVE les_picking TO les_header_picking.
        IF les_picking-aufnr is initial and les_picking-rsnum is initial.
*... Se borran los registros de posición del documento cerrado
        DELETE from zmm_T_picking_dt where picknum = les_header_picking-picknum .
        ELSE.
*... Asigno estado pendiente otra vez
          les_header_picking-estado = 'PENDIENTE'.
          Select single max( POSPICK )
            from zmm_T_picking_dt
            into lv_consecutivo
            where picknum = les_header_picking-picknum.
        ENDIF.

      ELSE.
      call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = wnorange
        object                  = wobjeto
        subobject               = wsubobj
      importing
        number                  = p_doc_number " Número generado por SAP
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        internal_overflow       = 6
        others                  = 7.

        les_header_picking-picknum = p_doc_number.
        les_header_picking-estado = 'PENDIENTE'.

        ENDIF.

      LOOP AT r_pos_picking into les_pos_picking where vbeln2 = les_header_picking-vbeln2 and RSNUM = les_header_picking-rsnum and CANTIDAD > 0.
          IF les_header_picking-aglutinador is not initial.
            CLEAR:les_picking_aux_dt.
*            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
*            rsnum = les_pos_picking-rsnum.
*... Se añade que la cantidad debe ser igual
            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
            rsnum = les_pos_picking-rsnum cantidad = les_pos_picking-cantidad.
            IF sy-subrc eq 0 .
              CONTINUE.
*            ELSE.
**... Se consulta la cantidad parcial
*            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
*            rsnum = les_pos_picking-rsnum.
            ENDIF.
          ENDIF.
          lv_consecutivo = lv_consecutivo + 1.

*          INICIO SO-133 LFY 14/04/2015.

*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            EXPORTING
*              input         =           les_pos_picking-material
*           IMPORTING
*             OUTPUT        =           les_pos_picking-material

*          FIN SO-133 LFY 14/04/2015.
                    .
*AD
*          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          IF les_pos_picking-cantidad  <= 0 .
              CONTINUE.
          ENDIF.
*AD
          les_pos_picking-picknum = les_header_picking-picknum.
          les_pos_picking-POSPICK = lv_consecutivo.
*MD          les_pos_picking-diferencia = les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-diferencia = les_pos_picking-cantidad.
          les_pos_picking-CANTCONT = 0.
          APPEND les_pos_picking to lti_picking_dt.

      ENDLOOP.

      MODIFY zmm_t_picking from les_header_picking.
      MODIFY zmm_T_picking_dt from TABLE lti_picking_dt.
    ENDLOOP.

  ENDIF.
ENDMETHOD.


  method CREATE_BY_PKBLE_REF_PVTA.
*{   INSERT         ER3K900279                                        1
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.


*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: lti_picking_aux_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

  DATA: les_picking_aux_dt TYPE zmm_t_picking_dt.

*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_pos_picking TYPE zedsd_picking_det.



DATA: lv_consecutivo type ZED_POSPICK.

   "Declarar variables
  data: wobjeto      type tnro-object    value 'ZEDPICKING', " Nombre del SNRO
        wnorange     type inri-nrrangenr value '01',        "Número de rango,
        wsubobj      type inri-subobject value space,       "Subobject
        w_doc_number type char10.

  data:  p_doc_number type char18. " Variable para almacenar el número generado


*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de picking
  CALL METHOD i_ref_pickable->get_header_picking
    RECEIVING
      r_header_picking = r_header_picking.

*... Válido que existan registros
  IF r_header_picking[] IS NOT INITIAL.

*... Consulto todos los registros que se hayan creado con anterioridad
    SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO
           UNAME FEMOD HOMOD
      FROM zmm_t_picking
      INTO  TABLE lti_picking
      FOR ALL ENTRIES IN  r_header_picking
      WHERE vbeln EQ r_header_picking-vbeln.

*... Llamo el metodo para Retornara la Estructura de detalle para un document de picking
    CALL METHOD i_ref_pickable->get_pos_picking
      RECEIVING
        r_pos_picking = r_pos_picking.

     IF lti_picking is not initial.
*       Consulto los detalles del picking
      SELECT MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
             UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
             AUFNR USUARIO
      FROM zmm_t_picking_dt
      INTO  TABLE lti_picking_aux_dt
      FOR ALL ENTRIES IN  lti_picking
      WHERE picknum eq lti_picking-picknum.

       "Filtro los documentos de picking cerrados
       LOOP AT lti_picking into les_picking.
         IF les_picking-estado = 'CERRADO'.
*... se borra registro cerrado de la cabecera
                    DELETE r_header_picking where vbeln2 = les_picking-vbeln2 and
                                       vbeln  = les_picking-vbeln.
*... Se borran los registros de posición del documento cerrado
                    DELETE r_pos_picking where picknum = les_picking-picknum .

         ENDIF.
       ENDLOOP.

     ENDIF.

*... Recorro las cabeceras de picking
    LOOP AT r_header_picking INTO les_header_picking.
      CLEAR:    lv_consecutivo ,les_picking.

*.... Busco si hay algun registro en la cabecera

     READ TABLE lti_picking into les_picking WITH KEY vbeln2 = les_header_picking-vbeln2 vbeln = les_header_picking-vbeln.
      IF sy-subrc EQ 0 .

        MOVE les_picking TO les_header_picking.
        IF les_picking-aufnr is initial and les_picking-rsnum is initial.
*... Se borran los registros de posición del documento cerrado
        DELETE from zmm_T_picking_dt where picknum = les_header_picking-picknum .
        ELSE.
*... Asigno estado pendiente otra vez
          les_header_picking-estado = 'PENDIENTE'.
          Select single max( POSPICK )
            from zmm_T_picking_dt
            into lv_consecutivo
            where picknum = les_header_picking-picknum.
        ENDIF.

      ELSE.
      call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = wnorange
        object                  = wobjeto
        subobject               = wsubobj
      importing
        number                  = p_doc_number " Número generado por SAP
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        internal_overflow       = 6
        others                  = 7.

        les_header_picking-picknum = p_doc_number.
        les_header_picking-estado = 'PENDIENTE'.

        ENDIF.

      LOOP AT r_pos_picking into les_pos_picking where vbeln2 = les_header_picking-vbeln2 and RSNUM = les_header_picking-rsnum and CANTIDAD > 0.
*          IF les_header_picking-aglutinador is not initial.
*            CLEAR:les_picking_aux_dt.
**            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
**            rsnum = les_pos_picking-rsnum.
**... Se añade que la cantidad debe ser igual
*            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
*            rsnum = les_pos_picking-rsnum cantidad = les_pos_picking-cantidad.
*            IF sy-subrc eq 0 .
*              CONTINUE.
**            ELSE.
***... Se consulta la cantidad parcial
**            READ TABLE lti_picking_aux_dt into les_picking_aux_dt WITH KEY posicion = les_pos_picking-posicion material = les_pos_picking-material
**            rsnum = les_pos_picking-rsnum.
*            ENDIF.
*          ENDIF.
          lv_consecutivo = lv_consecutivo + 1.

*          INICIO SO-133 LFY 14/04/2015.

*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            EXPORTING
*              input         =           les_pos_picking-material
*           IMPORTING
*             OUTPUT        =           les_pos_picking-material

*          FIN SO-133 LFY 14/04/2015.
                    .
*AD
*          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-cantidad =   les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          IF les_pos_picking-cantidad  <= 0 .
              CONTINUE.
          ENDIF.
*AD
          les_pos_picking-picknum = les_header_picking-picknum.
          les_pos_picking-POSPICK = lv_consecutivo.
*MD          les_pos_picking-diferencia = les_pos_picking-cantidad - les_pos_picking-CANTCONT.
          les_pos_picking-diferencia = les_pos_picking-cantidad.
          les_pos_picking-CANTCONT = 0.
          APPEND les_pos_picking to lti_picking_dt.

      ENDLOOP.

      MODIFY zmm_t_picking from les_header_picking.
      MODIFY zmm_T_picking_dt from TABLE lti_picking_dt.
    ENDLOOP.

  ENDIF.


*}   INSERT
  endmethod.


METHOD get_data_by_aglutinador.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum para Una
*                Orden de Prod. Con Aglutinador
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 08.09.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 08.09.2014   ER6K907457     Marco Suarez G    Creación
*-------------------------------------------------------------------------------*

  DATA: lti_cab_picking TYPE zttsd_cab_pickum_aglu.

  IF i_aglutinador IS NOT INITIAL.

*... Consulto para una Orden de Aglutiandor sus Ordenes Hijas
    SELECT picknum aglutinador rsnum aufnr estado
      FROM zmm_t_picking
      INTO  TABLE lti_cab_picking
      WHERE aglutinador EQ i_aglutinador.
*... Valido que se hayan encontrado coincidencias
    IF sy-subrc EQ 0.
*... Asigno numero de documentos de picking encontrados
      r_picknum_aglutinador  = lti_cab_picking.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_data_by_entrega.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por entrega
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

*... Consulto para una entrega que número de documento de picking tiene
  SELECT SINGLE picknum
    FROM zmm_t_picking
    INTO lv_picknum
    WHERE vbeln2 EQ i_entrega.
*... Valido que se hayan encontrado coincidencias
  IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
    e_picknum  = lv_picknum.
  ENDIF.

ENDMETHOD.


method GET_DATA_BY_ENTREGA_PND.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por entrega pendiente
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*{   REPLACE        ER3K900279                                        1
*\*... Variable para almacenar el número de documento de picking
*\  DATA: lv_picknum  TYPE zed_picknum.
*\
*\*... Consulto para una entrega que número de documento de picking tiene
*\  SELECT SINGLE picknum
*\    FROM zmm_t_picking
*\    INTO lv_picknum
*\    WHERE vbeln2 EQ i_entrega
*\    and estado ne 'CERRADO'.
*\*... Valido que se hayan encontrado coincidencias
*\  IF sy-subrc EQ 0.
*\*... Asigno numero de documento de picking
*\    e_picknum  = lv_picknum.
*\  ENDIF.
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

IF I_T_ENTREGAS is initial.
*... Consulto para una entrega que número de documento de picking tiene
  SELECT SINGLE picknum
    FROM zmm_t_picking
    INTO lv_picknum
    WHERE vbeln2 EQ i_entrega
    and estado ne 'CERRADO'.
*... Valido que se hayan encontrado coincidencias
  IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
    e_picknum  = lv_picknum.
  ENDIF.
ELSE.
   SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
          USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
    FROM zmm_t_picking
    INTO table e_t_picking
     FOR ALL ENTRIES IN I_T_ENTREGAS
    WHERE vbeln2 EQ I_T_ENTREGAS-entrega
    and estado ne 'CERRADO'.

ENDIF.

*}   REPLACE


endmethod.


METHOD GET_DATA_BY_ORDEN.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por entrega
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

IF i_orden is not initial.

*... Consulto para una entrega que número de documento de picking tiene
*{   REPLACE        ER3K900304                                        1
*\  SELECT SINGLE picknum
*\    FROM zmm_t_picking
*\    INTO lv_picknum
*\    WHERE aufnr EQ i_orden.
  SELECT SINGLE picknum
    FROM zmm_t_picking
    INTO lv_picknum
    WHERE aufnr EQ i_orden and
          aglutinador eq ' '.
*}   REPLACE
*... Valido que se hayan encontrado coincidencias
  IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
    e_picknum  = lv_picknum.
  ENDIF.
  ENDIF.
ENDMETHOD.


  method GET_DATA_BY_ORDEN_AG.
*{   INSERT         ER3K900279                                        1
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por entrega
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

IF i_orden is not initial.

*... Consulto para una entrega que número de documento de picking tiene
*{   REPLACE        ER3K900304                                        1
*\  SELECT SINGLE picknum
*\    FROM zmm_t_picking
*\    INTO lv_picknum
*\    WHERE aufnr EQ i_orden.
  SELECT SINGLE picknum
    FROM zmm_t_picking
    INTO lv_picknum
    WHERE aufnr EQ i_orden and
          aglutinador ne ' '.
*}   REPLACE
*... Valido que se hayan encontrado coincidencias
  IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
    e_picknum  = lv_picknum.
  ENDIF.
  ENDIF.
*}   INSERT
  endmethod.


  method GET_DATA_BY_ORDEN_B.
*{   INSERT         ER3K900279                                        1
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por orden
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

IF i_orden is not initial.

*... Consulto para una entrega que número de documento de picking tiene

  SELECT SINGLE picknum
    FROM zmm_t_picking
    INTO lv_picknum
    WHERE aufnr EQ i_orden .
*... Valido que se hayan encontrado coincidencias
  IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
    e_picknum  = lv_picknum.
  ENDIF.
  ENDIF.

*}   INSERT
  endmethod.


METHOD GET_DATA_BY_PEDIDO.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
*{   REPLACE        ER3K900279                                        1
*\* Descripción  : Metodo que retorna un picknum por entrega
* Descripción  : Metodo que retorna un picknum por pedido
*}   REPLACE
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

*{   REPLACE        ER3K900279                                        1
*\*... Consulto para una entrega que número de documento de picking tiene
*\  SELECT SINGLE picknum
*\    FROM zmm_t_picking
*\    INTO lv_picknum
*\    WHERE vbeln2 EQ i_entrega.
*\*... Valido que se hayan encontrado coincidencias
*\  IF sy-subrc EQ 0.
*\*... Asigno numero de documento de picking
*\    e_picknum  = lv_picknum.
*\  ENDIF.
*... Consulto para un pedido que número de documento de picking tiene
   SELECT MANDT PICKNUM  PROCESO SUBPROCESO VBELN  EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
    FROM zmm_t_picking
    INTO table C_PICKING_CAB_SUP
    WHERE EBELN EQ i_pedido AND
     estado EQ 'CERRADO'
     .

*}   REPLACE

ENDMETHOD.


  METHOD get_data_by_pkble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Consultar Cabeceras y Detalles de Documentos Picking
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 21.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 21.07.2014    ER6K907021    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

    DATA:
*{   INSERT         ER3K900279                                        1
    c_picking_det_sup_x type ZTTSD_PICKING_DET_SUP,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos LIKE LINE OF c_picking_det_sup_x,
*}   INSERT
*.... Estructura para retorno de datos de usuario
        E_DET_USUARIO type ZMM_T_IMP_ETIQ          ,
        E_DET_USUARIO_pck type ZMM_T_IMP_ETIQ          ,
        c_mensajes type ZTTSD_MSG_PICKING,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle type table of zesd_eanmat,
           lti_mensajes type table of zesd_msg_picking,
*.....Tabla Interna con informacion de Documentos Picking
         lti_header_picking TYPE zttsd_picking.
*.... Estructura  asignar al field symbol para la entrega
    DATA: les_entrega_picking TYPE zesd_entrega_picking.
*.... Estructura  asignar al field symbol de posiciónes de un documento de picking
    DATA: les_pos_picking TYPE zedsd_picking_det_sup.

    DATA: les_tab type sy-tabix,
          lti_mara TYPE TABLE OF mara,
          les_mara TYPE mara.

    FIELD-SYMBOLS:
<lfs_detalle> type zesd_eanmat,
*.....Estructura Tabla Interna Picking detalle
         <lfs_pos_picking> TYPE zedsd_picking_det_sup,
*.....Estructura Tabla Interna Picking
         <lfs_header_picking> TYPE zedsd_picking,
*....Estructura con Entregas-Picking
         <lfs_entrega_picking> TYPE zesd_entrega_picking.
*... Asigno estructuras a los fieldsymbol
    ASSIGN les_entrega_picking TO <lfs_entrega_picking>.
*    ASSIGN les_pos_picking to <lfs_pos_picking>.
*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de picking
    CALL METHOD i_ref_pickable->get_header_picking
      RECEIVING
        r_header_picking = lti_header_picking.

*... Válido que existan registros
    IF lti_header_picking[] IS NOT INITIAL.
*.....Ordeno Tabla a Procesar
      SORT lti_header_picking BY vbeln ebeln aglutinador ASCENDING.
*.....Consulto Tabla Cabeceras Picking
*{   REPLACE        ER3K900279                                        4
*\      SELECT *
*\          FROM zmm_t_picking
*\              INTO  CORRESPONDING FIELDS OF TABLE c_picking_cab_sup
*\             FOR ALL ENTRIES IN  lti_header_picking
*\                         WHERE vbeln EQ lti_header_picking-vbeln AND
*\                               ebeln EQ lti_header_picking-ebeln AND
*\                               aglutinador EQ lti_header_picking-aglutinador AND
*\                               vbeln2 EQ lti_header_picking-vbeln2 AND
*\                               aufnr EQ lti_header_picking-aufnr AND
*\                               rsnum EQ lti_header_picking-rsnum.
      SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
             USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
          FROM zmm_t_picking
              INTO  TABLE c_picking_cab_sup
             FOR ALL ENTRIES IN  lti_header_picking
                         WHERE vbeln EQ lti_header_picking-vbeln AND
                               ebeln EQ lti_header_picking-ebeln AND
                               aglutinador EQ lti_header_picking-aglutinador AND
                               vbeln2 EQ lti_header_picking-vbeln2 AND
                               aufnr EQ lti_header_picking-aufnr AND
                               rsnum EQ lti_header_picking-rsnum.
*}   REPLACE
*..... Filtro para los documentos de venta (filtrar por centro y almacén )
        IF i_tipo_documento eq 'PVTA'.
*..... Consulto el centro y almacén del usuario que realiza el picking
          CALL METHOD zcl_lgtica_util=>get_detalles_usuario
            EXPORTING
              i_usuario     = i_usuario
            IMPORTING
              e_det_usuario = e_deT_usuario
            CHANGING
              c_mensajes    = c_mensajes
              .
*.....Recorro todos los picking y elimino todo el que no sea del centro y almacen del usuario que realiza el picking
          LOOP AT c_picking_cab_sup ASSIGNING <lfs_header_picking>.
            CLEAR:E_DET_USUARIO_pck.
            les_tab = sy-tabix.
*..... Consulto el centro y almacén del usuario del picking
          CALL METHOD zcl_lgtica_util=>get_detalles_usuario
            EXPORTING
              i_usuario     = <lfs_header_picking>-usuario
            IMPORTING
              e_det_usuario = E_DET_USUARIO_pck
            CHANGING
              c_mensajes    = c_mensajes
              .
*..... Si el centro no es igual no se envía el picking
              IF E_DET_USUARIO_pck-CENTRO ne E_DET_USUARIO-CENTRO.
                delete c_picking_cab_sup INDEX les_tab.
              ENDIF.
           ENDLOOP.
        ENDIF.
*.....Indicador
      i_indicador = 'X'.

      IF c_picking_cab_sup is not initial AND i_indicador IS NOT INITIAL.
*.....Ordeno la Tabla a Procesar
        SORT c_picking_cab_sup BY picknum ASCENDING.
*.....Consulto Tabla Posiciones Picking
*{   REPLACE        ER3K900279                                        5
*\        SELECT *
*\           FROM zmm_t_picking_dt
*\              INTO CORRESPONDING FIELDS OF TABLE c_picking_det_sup
*\                  FOR ALL ENTRIES IN  c_picking_cab_sup
*\                      WHERE picknum EQ c_picking_cab_sup-picknum.
        SELECT MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
               UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
               AUFNR USUARIO
           FROM zmm_t_picking_dt
              INTO TABLE c_picking_det_sup
                  FOR ALL ENTRIES IN  c_picking_cab_sup
                      WHERE picknum EQ c_picking_cab_sup-picknum.
*}   REPLACE
        IF sy-subrc EQ 0.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT c_picking_det_sup ASSIGNING <lfs_pos_picking>.
            les_mara-matnr = <lfs_pos_picking>-material.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

          LOOP AT c_picking_det_sup ASSIGNING <lfs_pos_picking>.
*.....Limpieza de Variables
              clear : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
              call function 'Z_SD_CONSUL_EAN_MATE'
                exporting
                  matnr      = <lfs_pos_picking>-material
                tables
                  t_detalle  = lti_detalle
                  t_mensajes = lti_mensajes.

             CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input                = <lfs_pos_picking>-umc
                language             = 'S'
             IMPORTING
*               LONG_TEXT            =
               output               = <lfs_pos_picking>-umc
*               SHORT_TEXT           =
             EXCEPTIONS
               unit_not_found       = 1
               OTHERS               = 2.

           read table  lti_detalle assigning <lfs_detalle> with key UNIDAD_MEDIDA = <lfs_pos_picking>-umc.
           IF sy-subrc ne 0 .
             CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input                = <lfs_pos_picking>-umc
                language             = 'S'
             IMPORTING
*               LONG_TEXT            =
               output               = <lfs_pos_picking>-umc
*               SHORT_TEXT           =
             EXCEPTIONS
               unit_not_found       = 1
               OTHERS               = 2
                      .
           ENDIF.

            IF <lfs_pos_picking>-umc EQ '004'.
              <lfs_pos_picking>-umc = 'UN'.
            ENDIF.
            <lfs_pos_picking>-umd = <lfs_pos_picking>-umc.
            <lfs_pos_picking>-umcc = <lfs_pos_picking>-umc.
*{   INSERT         ER3K900279                                        2
            IF I_FLAG_AGRUP EQ 'X'.
            MOVE-CORRESPONDING <lfs_pos_picking> to les_det_pos.
            clear:les_det_pos-POSICION , les_det_pos-POSPICK.
            COLLECT les_det_pos INTO C_PICKING_DET_SUP_X.
            MODIFY C_PICKING_DET_SUP_X from les_det_pos index sy-tabix TRANSPORTING CONTEO.
            ENDIF.
*}   INSERT

          ENDLOOP.
*{   INSERT         ER3K900279                                        3
           IF I_FLAG_AGRUP EQ 'X'.
            C_PICKING_DET_SUP = C_PICKING_DET_SUP_X.
            ENDIF.
*}   INSERT

*.....Lleno Tabla Entregas-Picking
          LOOP AT c_picking_cab_sup ASSIGNING <lfs_header_picking>.
            <lfs_entrega_picking>-rsnum = <lfs_header_picking>-rsnum .
            <lfs_entrega_picking>-aufnr = <lfs_header_picking>-aufnr .
            <lfs_entrega_picking>-vbeln2 = <lfs_header_picking>-vbeln2 .
            <lfs_entrega_picking>-picknum =  <lfs_header_picking>-picknum.
            APPEND  <lfs_entrega_picking>  TO c_entrega_picking.
          ENDLOOP.
        ENDIF.

*... Consulto los seriales para los detalles del documento de picking
*{   REPLACE        ER3K900279                                        6
*\        SELECT * FROM zmm_t_pck_ser
*\          INTO CORRESPONDING FIELDS OF TABLE c_picking_ser
*\          FOR ALL ENTRIES IN c_picking_cab_sup
*\*          WHERE picknum EQ i_picknum
*\          WHERE    ( vbeln EQ c_picking_cab_sup-vbeln AND vbeln NE '') OR
*\                   ( ebeln EQ c_picking_cab_sup-ebeln AND ebeln NE '') OR
*\                   ( aglutinador EQ c_picking_cab_sup-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ c_picking_cab_sup-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ c_picking_cab_sup-rsnum AND rsnum NE '').
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER
               UBICACION_FI
          FROM zmm_t_pck_ser
          INTO TABLE c_picking_ser
          FOR ALL ENTRIES IN c_picking_cab_sup
*          WHERE picknum EQ i_picknum
          WHERE    ( vbeln EQ c_picking_cab_sup-vbeln AND vbeln NE '') OR
                   ( ebeln EQ c_picking_cab_sup-ebeln AND ebeln NE '') OR
                   ( aglutinador EQ c_picking_cab_sup-aglutinador AND aglutinador NE '') OR
                   ( aufnr EQ c_picking_cab_sup-aufnr AND aufnr NE '' ) OR
                   ( rsnum EQ c_picking_cab_sup-rsnum AND rsnum NE '').
*}   REPLACE

*... Consulto los lotes para los detalles del documento de picking
*{   REPLACE        ER3K900279                                        7
*\        SELECT * FROM zmm_t_pck_lot
*\          INTO CORRESPONDING FIELDS OF TABLE c_picking_lot
*\          FOR ALL ENTRIES IN c_picking_cab_sup
*\*          WHERE picknum EQ i_picknum
*\          WHERE    ( vbeln EQ c_picking_cab_sup-vbeln AND vbeln NE '') OR
*\                   ( ebeln EQ c_picking_cab_sup-ebeln AND ebeln NE '') OR
*\                   ( aglutinador EQ c_picking_cab_sup-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ c_picking_cab_sup-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ c_picking_cab_sup-rsnum AND rsnum NE '').
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR  LOTE UBICACION_FI CANTIDAD UMC VBELN EBELN AGLUTINADOR
               VBELN2 AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
          FROM zmm_t_pck_lot
          INTO TABLE c_picking_lot
          FOR ALL ENTRIES IN c_picking_cab_sup
*          WHERE picknum EQ i_picknum
          WHERE    ( vbeln EQ c_picking_cab_sup-vbeln AND vbeln NE '') OR
                   ( ebeln EQ c_picking_cab_sup-ebeln AND ebeln NE '') OR
                   ( aglutinador EQ c_picking_cab_sup-aglutinador AND aglutinador NE '') OR
                   ( aufnr EQ c_picking_cab_sup-aufnr AND aufnr NE '' ) OR
                   ( rsnum EQ c_picking_cab_sup-rsnum AND rsnum NE '').
*}   REPLACE
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD get_data_by_rsnum.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que retorna un picknum por entrega
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Variable para almacenar el número de documento de picking
  DATA: lv_picknum  TYPE zed_picknum.

  IF i_rsnum IS NOT INITIAL.
*... Consulto para una entrega que número de documento de picking tiene
    SELECT SINGLE picknum
      FROM zmm_t_picking
      INTO lv_picknum
      WHERE rsnum EQ i_rsnum.
*... Valido que se hayan encontrado coincidencias
    IF sy-subrc EQ 0.
*... Asigno numero de documento de picking
      e_picknum  = lv_picknum.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD load_picking.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Carga los datos de un documento picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*

*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.
*... Estructura interna para la posicion del documento de picking
  DATA: les_picking_dt TYPE  zmm_t_picking_dt.

*... tabla interna para los seriales de un documento de picking
  DATA: lti_picking_ser TYPE STANDARD TABLE OF zmm_t_pck_ser.
*... Estructura interna para la posicion de los seriales de un documento de picking
  DATA: les_picking_ser TYPE  zmm_t_pck_ser.

*... tabla interna para los lotes de un documento de picking
  DATA: lti_picking_lot TYPE STANDARD TABLE OF zmm_t_pck_lot.
*... Estructura interna para la posicion de los lotes de un documento de picking
  DATA: les_picking_lot TYPE  zmm_t_pck_lot.

*CALL METHOD zcl_lgtica_util=>ajustar_documento
*  EXPORTING
*    i_documento = i_picknum
*  receiving
*    r_documento = i_picknum
*    .

*  set de atributo picknum
  picknum = i_picknum.
*... Valido que se haya ingresado un numero de documento de picking
  IF i_picknum IS NOT INITIAL.
*.... Consulto la cabecera para los documento de picking
*{   REPLACE        ER3K900279                                        1
*\    SELECT *  FROM zmm_t_picking
*\      INTO TABLE lti_picking
*\      WHERE picknum EQ i_picknum.
    SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
           USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_picking
      INTO TABLE lti_picking
      WHERE picknum EQ i_picknum.
*}   REPLACE
*... valido que hayan encontrado registros de cabecera
    IF lti_picking[] IS NOT INITIAL.
*... cargo en la estructura el registro encontrado de la cabecera del documento de picking
      READ TABLE lti_picking INTO les_picking INDEX 1.
      IF sy-subrc EQ 0.
*... Realizo set del atributo de cabecera del documento de picking
        s_picking = les_picking.
      ENDIF.

*... Consulto el detalle para los documento de picking encontrado
*{   REPLACE        ER3K900279                                        2
*\      SELECT  * FROM zmm_t_picking_dt
*\        INTO TABLE lti_picking_dt
*\        FOR ALL ENTRIES IN lti_picking
*\        WHERE picknum EQ lti_picking-picknum.
      SELECT  MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
              UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
              AUFNR USUARIO
        FROM zmm_t_picking_dt
        INTO TABLE lti_picking_dt
        FOR ALL ENTRIES IN lti_picking
        WHERE picknum EQ lti_picking-picknum.
*}   REPLACE
*... valido que hayan encontrado registros de detalle del documento de picking
      IF lti_picking_dt[] IS NOT INITIAL.
*... Realizo set del atributo de detalle del documento de picking
        t_picking_det = lti_picking_dt.
*... Consulto los seriales para los detalles del documento de picking
*{   REPLACE        ER3K900279                                        3
*\        SELECT * FROM zmm_t_pck_ser
*\          INTO TABLE lti_picking_ser
*\          FOR ALL ENTRIES IN lti_picking
*\*          WHERE picknum EQ i_picknum
*\          WHERE    ( vbeln EQ lti_picking-vbeln and vbeln ne '') OR
*\                   ( ebeln EQ lti_picking-ebeln and ebeln ne '') OR
*\                   ( aglutinador EQ lti_picking-aglutinador and aglutinador ne '') or
*\                   ( aufnr EQ lti_picking-aufnr and aufnr ne '' ) OR
*\                   ( rsnum EQ lti_picking-rsnum and rsnum ne '').
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER
               UBICACION_FI FROM zmm_t_pck_ser
          INTO TABLE lti_picking_ser
          FOR ALL ENTRIES IN lti_picking
*          WHERE picknum EQ i_picknum
          WHERE    ( vbeln EQ lti_picking-vbeln and vbeln ne '') OR
                   ( ebeln EQ lti_picking-ebeln and ebeln ne '') OR
                   ( aglutinador EQ lti_picking-aglutinador and aglutinador ne '') or
                   ( aufnr EQ lti_picking-aufnr and aufnr ne '' ) OR
                   ( rsnum EQ lti_picking-rsnum and rsnum ne '').
*}   REPLACE
        IF sy-subrc EQ 0.
*... Realizo set del atributo de seriales del documento de picking
          t_picking_ser = lti_picking_ser.
        ENDIF.

*... Consulto los lotes para los detalles del documento de picking
*{   REPLACE        ER3K900279                                        4
*\        SELECT * FROM zmm_t_pck_lot
*\          INTO TABLE lti_picking_lot
*\          FOR ALL ENTRIES IN lti_picking
*\*          WHERE picknum EQ i_picknum
*\      WHERE    ( vbeln EQ lti_picking-vbeln and vbeln ne '') OR
*\                   ( ebeln EQ lti_picking-ebeln and ebeln ne '') OR
*\                   ( aglutinador EQ lti_picking-aglutinador and aglutinador ne '') or
*\                   ( aufnr EQ lti_picking-aufnr and aufnr ne '' ) OR
*\                   ( rsnum EQ lti_picking-rsnum and rsnum ne '').
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR  LOTE UBICACION_FI CANTIDAD UMC VBELN EBELN AGLUTINADOR
               VBELN2 AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
          FROM zmm_t_pck_lot
          INTO TABLE lti_picking_lot
          FOR ALL ENTRIES IN lti_picking
*          WHERE picknum EQ i_picknum
      WHERE    ( vbeln EQ lti_picking-vbeln and vbeln ne '') OR
                   ( ebeln EQ lti_picking-ebeln and ebeln ne '') OR
                   ( aglutinador EQ lti_picking-aglutinador and aglutinador ne '') or
                   ( aufnr EQ lti_picking-aufnr and aufnr ne '' ) OR
                   ( rsnum EQ lti_picking-rsnum and rsnum ne '').
*}   REPLACE
        IF sy-subrc EQ 0.
*... Realizo set del atributo de seriales del documento de picking
          t_picking_lot = lti_picking_lot.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


method MODIFY_PICKING_BY_RECIBO.
endmethod.


METHOD set_create_by_pkble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
  DATA: lv_cantser TYPE i.
*{   REPLACE        ER3K900279                                        5
*\  DATA: lv_cantmat TYPE i.
  DATA: lv_cantmat TYPE ZEDSD_CANT_DOC.
*}   REPLACE
*.....Variable de Ref a la Clase Picknum
  DATA:   lo_lgtica_picking TYPE REF TO zcl_lgtica_picking.
*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.
*... tabla interna para la posicion del documento de picking tabla Z
  DATA: lti_zmm_t_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.

*... Estructura para posición del documento de picking tabla Z
  DATA: les_picking_dt TYPE zmm_t_picking_dt.
*... Estructura para posición del documento de picking tabla Z
  DATA: les_picking_dt_aux TYPE zmm_t_picking_dt.
*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_pos_picking TYPE zedsd_picking_det.
*... Estructura para posición de seriales documento de picking
  DATA: les_picking_ser TYPE zedsd_picking_ser.
*... Estructura para posición de lotes documento de picking
  DATA: les_picking_lot TYPE zedsd_picking_lot.

  DATA: lv_consecutivo TYPE zed_pospick.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPICKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado


*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de picking
  CALL METHOD i_ref_pickable->get_header_picking
    RECEIVING
      r_header_picking = r_header_picking.

*... Válido que existan registros
  IF r_header_picking[] IS NOT INITIAL.

*{   REPLACE        ER3K900279                                        3
*\*... Consulto todos los registros que se hayan creado con anterioridad
*\    SELECT *
*\      FROM zmm_t_picking
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_picking
*\      FOR ALL ENTRIES IN  r_header_picking
*\      WHERE vbeln2 EQ r_header_picking-vbeln2
*\      AND vbeln EQ r_header_picking-vbeln
*\      AND ebeln EQ r_header_picking-ebeln
*\      AND aglutinador EQ r_header_picking-aglutinador
*\      AND aufnr EQ r_header_picking-aufnr
*\      AND rsnum EQ r_header_picking-rsnum.
*... Consulto todos los registros que se hayan creado con anterioridad
    SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_picking
      INTO  TABLE lti_picking
      FOR ALL ENTRIES IN  r_header_picking
      WHERE vbeln2 EQ r_header_picking-vbeln2
      AND vbeln EQ r_header_picking-vbeln
      AND ebeln EQ r_header_picking-ebeln
      AND aglutinador EQ r_header_picking-aglutinador
      AND aufnr EQ r_header_picking-aufnr
      AND rsnum EQ r_header_picking-rsnum.
*}   REPLACE
*... Llamo el metodo para Retornara la Estructura de detalle para un document de picking
    CALL METHOD i_ref_pickable->get_pos_picking
      RECEIVING
        r_pos_picking = r_pos_picking.

    IF lti_picking IS NOT INITIAL.
*{   REPLACE        ER3K900279                                        4
*\      SELECT *
*\        INTO  CORRESPONDING FIELDS OF TABLE lti_zmm_t_picking_dt
*\        FROM zmm_t_picking_dt
*\        FOR ALL ENTRIES IN lti_picking
*\        WHERE  picknum EQ lti_picking-picknum.
      SELECT MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
             UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO
        INTO  TABLE lti_zmm_t_picking_dt
        FROM zmm_t_picking_dt
        FOR ALL ENTRIES IN lti_picking
        WHERE  picknum EQ lti_picking-picknum.
*}   REPLACE


      "Filtro los documentos de picking cerrados
      LOOP AT lti_picking INTO les_picking.
        IF les_picking-estado = 'CERRADO' .
*... se borra registro cerrado de la cabecera
          DELETE r_header_picking WHERE vbeln2 = les_picking-vbeln2 AND
                             vbeln  = les_picking-vbeln AND
                             ebeln  = les_picking-ebeln AND
                             aglutinador = les_picking-aglutinador AND
                             aufnr = les_picking-aufnr AND
                             rsnum = les_picking-rsnum.
*... Se borran los registros de posición del documento cerrado
          DELETE r_pos_picking WHERE picknum = les_picking-picknum .

        ENDIF.
      ENDLOOP.

    ENDIF.

*... Ordeno la tabla de posiciones para el documento de picking
    SORT lti_zmm_t_picking_dt BY pospick DESCENDING.
*... Leo el primer registro para obtener el consecutivo
    READ TABLE lti_zmm_t_picking_dt INTO les_picking_dt INDEX 1.
    IF sy-subrc EQ 0.
      lv_consecutivo = les_picking_dt-pospick.
    ENDIF.
*... Valido los registros de la tabla Z
    IF lti_zmm_t_picking_dt[] IS NOT INITIAL.

      SORT lti_zmm_t_picking_dt BY posicion p_confirm DESCENDING.

      CLEAR:lti_picking_dt.
*.... Recorro los registros con posiciones marcadas como novedad menor
      LOOP AT lti_zmm_t_picking_dt INTO les_picking_dt WHERE p_confirm = 'NE'.
*... Limpio variables
        CLEAR:lv_cantser.
*.....Creo Instancia de Clase
        IF  lo_lgtica_picking IS NOT BOUND  .
          CREATE OBJECT lo_lgtica_picking .
        ENDIF.
*.....Cargo Atributos Picking
        lo_lgtica_picking->load_picking(
          EXPORTING
            i_picknum = les_picking_dt-picknum ).


*... Guardo la cantidad contada del material
        lv_cantmat = les_picking_dt-cantcont.
*... Recorro los seriales que no han sido usados
        LOOP AT lo_lgtica_picking->t_picking_ser INTO les_picking_ser WHERE matnr = les_picking_dt-material AND flag_ser NE 'X'.

          IF lv_cantser < lv_cantmat.
*... Elimino la posicion del serial
            DELETE zmm_t_pck_ser FROM les_picking_ser.
            lv_cantser = lv_cantser + 1.
          ENDIF.
        ENDLOOP.

*... Recorro los lotes con cantidad de uso mayor a 0 y descuento
        LOOP AT lo_lgtica_picking->t_picking_lot INTO les_picking_lot WHERE matnr = les_picking_dt-material and
          ubicacion_fi eq les_picking_dt-ubicacion_fi and cantidad_uso > 0.

          IF lv_cantmat <= les_picking_lot-cantidad_uso and lv_cantmat > 0.
            les_picking_lot-cantidad_uso = les_picking_lot-cantidad_uso - lv_cantmat.
            les_picking_lot-cantidad = les_picking_lot-cantidad - lv_cantmat.
            lv_cantmat = 0.
*{   REPLACE        ER3K900279                                        1
*\            IF les_picking_lot-cantidad_uso EQ 0.
            IF les_picking_lot-cantidad_uso EQ 0 and les_picking_lot-cantidad EQ 0.
*}   REPLACE
*... Elimino la posicion del lote
            DELETE zmm_t_pck_lot FROM les_picking_lot.
            ELSE.
*... Actualizo la posicion del lote
            MODIFY zmm_t_pck_lot from les_picking_lot.
            ENDIF.

            EXIT.
          ELSE.
*... Actualizo la cantidad
            lv_cantmat = lv_cantmat - les_picking_lot-cantidad_uso.
*... Elimino la posicion del lote
            DELETE zmm_t_pck_lot FROM les_picking_lot.
          ENDIF.
        ENDLOOP.

*... Elimino todas las posiciones de la tabla Z
*{   REPLACE        ER3K900279                                        2
*\        DELETE FROM zmm_t_picking_dt WHERE picknum = les_picking_dt-picknum AND
*\        posicion = les_picking_dt-posicion.
        DELETE FROM zmm_t_picking_dt WHERE picknum = les_picking_dt-picknum AND
        posicion = les_picking_dt-posicion and pospick = les_picking_dt-POSPICK.
*}   REPLACE

*... Recorro los registros originales y los agrego
        LOOP AT r_pos_picking INTO les_pos_picking WHERE vbeln2 = les_picking_dt-vbeln2 AND rsnum = les_picking_dt-rsnum AND aufnr = les_picking_dt-aufnr
        AND posicion = les_picking_dt-posicion.
**... Consulto los EANS de el material
*    CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*       EXPORTING
*         matnr            = les_pos_picking-material
*        TABLES
**       T_MARA           =
*          t_detalle        = t_detalle
*          t_mensajes       = t_mensajes.
*
**... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
*    READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_pos_picking-umcc.
*    IF sy-subrc EQ 0.
*      les_pos_picking-cantidad = les_pos_picking-cantidad /  les_detalle-cantidad_ean.
*      les_pos_picking-cantcont = les_pos_picking-cantcont / les_detalle-cantidad_ean.
*    ENDIF.
          lv_consecutivo = lv_consecutivo + 1.
          les_pos_picking-picknum = les_picking_dt-picknum.
          les_pos_picking-pospick = lv_consecutivo.
          les_pos_picking-diferencia = les_pos_picking-cantidad - les_pos_picking-cantcont.
*{   INSERT         ER3K900279                                        6
          les_pos_picking-cantidad = les_pos_picking-diferencia.
          les_pos_picking-cantcont = 0.
*}   INSERT
          CLEAR:les_picking_dt_aux.
          READ TABLE lti_picking_dt INTO les_picking_dt_aux WITH KEY picknum = les_picking_dt-picknum posicion = les_pos_picking-posicion.
          IF les_picking_dt_aux IS INITIAL.
            APPEND les_pos_picking TO lti_picking_dt.
          ENDIF.

        ENDLOOP.

      ENDLOOP.
      MODIFY zmm_t_picking_dt FROM TABLE lti_picking_dt.
    ENDIF.


  ENDIF.
ENDMETHOD.


METHOD update_picking_by_cant.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Distribuye Registros en Tabla de posiciones Picking
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
DATA: le_flg_aglutinador TYPE C.
*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.
*... tabla interna para la cabecera del documento de picking
  DATA: lti_picking TYPE STANDARD TABLE OF zmm_t_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_picking TYPE zmm_t_picking.
*... tabla interna para la posicion del documento de picking
  DATA: lti_picking_dt TYPE STANDARD TABLE OF zmm_t_picking_dt.
*... Estructura para posición del documento de picking
  DATA: les_picking_dt TYPE zedsd_picking_det.

  DATA: l_e_matnr TYPE matnr.

  DATA: les_cab_posiciones TYPE zedsd_picking_det.
*... Cabecera de posicion documento de picking
  DATA: r_pos_picking TYPE zttsd_picking_det.
*... Estructura para posición del documento de picking
  DATA: les_reg_posiciones TYPE zedsd_picking_det.

*.... Posición en el loop
  DATA: lv_index TYPE sy-tabix.

*.... Cantidad diferencia total
  DATA: lv_cantdif_tot TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcontcab_umb TYPE zedsd_cant_cnt.

*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcont_umb TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositio para un material UMD
  DATA: lv_cantcont_umd TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositivo para un material UMB
  DATA: lv_cantdif_umb TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositio para un material UMD
  DATA: lv_cantdif_umd TYPE zedsd_cant_cnt.

  DATA: lv_consecutivo TYPE zed_pospick VALUE 1000.

**... Campos Errores en Procesamiento Cabecera
  DATA: les_msg TYPE zesd_msg_picking.
*.....Variable Texto Error
  DATA:          l_e_desc_error TYPE string.

*.....Variable Texto cantidad excedida Error
  DATA:          l_e_cexc_error TYPE string.
*.... Tablas auxiliares para guardar la información
  DATA: lti_cab_reg_posiciones TYPE zttsd_picking_det,
        lti_reg_posiciones TYPE zttsd_picking_det.

*... Guardo en las tablas auxiliares los valores por defecto
  lti_reg_posiciones  = c_reg_posiciones.
  lti_cab_reg_posiciones = c_cab_posiciones.

*{   INSERT         ER3K900279                                        2
    DATA: lv_p_confirm_lot TYPE zed_flag_confirm.
*}   INSERT

  DATA: lv_p_confirm TYPE zed_flag_confirm.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.
**... Elimino los registros que ya esten procesados cantidad diferencia = 0
**  DELETE lti_picking_dt WHERE diferencia = 0.
**... Inserción de seriales y lotes.
*
*MODIFY  ZMM_T_PCK_SER FROM TABLE c_picking_ser.
*MODIFY  ZMM_T_PCK_LOT FROM TABLE c_picking_lot.
*{   DELETE         ER3K900279                                        1
*\  CALL METHOD zcl_lgtica_picking=>add_ser_lot
*\    EXPORTING
*\      i_documento_origen = i_documento_origen
*\      i_entrega          = i_entrega
*\      i_tipo_documento   = i_tipo_documento
*\      i_clase_documento  = i_clase_documento
*\      i_usuario          = i_usuario
*\      c_mensajes         = c_mensajes
*\    CHANGING
*\      c_picking_ser      = c_picking_ser
*\      c_picking_lot      = c_picking_lot.
*}   DELETE

  sort c_reg_posiciones by p_confirm ascending.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
  LOOP AT c_reg_posiciones INTO les_reg_posiciones where p_confirm NE 'A'.
    les_mara-matnr = les_reg_posiciones-material.
    APPEND les_mara TO lti_mara.
  ENDLOOP.

*.....Función para Obtener EANs de un Material
  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
    TABLES
      t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

  LOOP AT c_reg_posiciones INTO les_reg_posiciones where p_confirm NE 'A'.
    CLEAR:les_picking_dt ,lv_cantdif_tot,lv_cantcontcab_umb.

*... consulto el registro de cabecera del dispositivo
    READ TABLE c_cab_posiciones INTO les_cab_posiciones WITH KEY material = les_reg_posiciones-material.

*... Copio la información de la tabla de posiciones del documento de picking a una tabla auxiliar
    lti_picking_dt = c_picking_det_sup.
*.....Función para Ajuste de Número
*    INICIO SO-133 LFY 14/2014

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " se descomenta esta parte del código, SO-133 LFY 14/2014
    EXPORTING
      input  = les_reg_posiciones-material
    IMPORTING
      output = l_e_matnr.

*... filtro solo los materiales
    DELETE lti_picking_dt WHERE material NE l_e_matnr.
*    DELETE lti_picking_dt WHERE material NE les_reg_posiciones-material.

*    FIN SO-133 LFY 14/2014
*... Es un Aglutinador
    IF les_reg_posiciones-aufnr IS NOT INITIAL.
      DELETE lti_picking_dt WHERE aufnr NE les_reg_posiciones-aufnr.
      le_flg_aglutinador = 'X'.
    ENDIF.
*... Ordeno la tabla de posiciones para el documento de picking
    SORT c_picking_det_sup BY pospick DESCENDING.
*... Leo el primer registro para obtener el consecutivo
    READ TABLE c_picking_det_sup INTO les_picking_dt INDEX 1.
    IF sy-subrc EQ 0.
      lv_consecutivo = les_picking_dt-pospick.
    ENDIF.

*...filtro los que tengan diferencia o cantidad para contar disponible
    DELETE lti_picking_dt WHERE diferencia = 0 AND p_confirm = 'OK' OR p_confirm = 'CT'.

    CLEAR:t_detalle.
*... Consulto los EANS de el material
    CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
       EXPORTING
         matnr            = les_reg_posiciones-material
        TABLES
*       T_MARA           =
          t_detalle        = t_detalle
          t_mensajes       = t_mensajes
                .
*... Acumulo la cantidad que puedo contar
    LOOP AT lti_picking_dt INTO les_picking_dt.
      lv_cantdif_tot = lv_cantdif_tot + les_picking_dt-diferencia.
    ENDLOOP.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
    IF les_picking_dt-rsnum IS NOT INITIAL .
*... Si tiene reserva se toma la unidad de medida de la posición de picking
      READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_picking_dt-umc.
    ELSE.
*... Si no tiene reserva se toma la unidad de medida de la cabecera del dispositivo
      READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_cab_posiciones-umcc.
    ENDIF.

    IF sy-subrc EQ 0.
      lv_cantcontcab_umb = les_cab_posiciones-cantcont /  les_detalle-cantidad_ean.
    ENDIF.
    les_reg_posiciones-p_confirm = ''.
*... Valido si se conto mas de lo que puedo contar
    IF lv_cantcontcab_umb <= lv_cantdif_tot.

      IF lv_cantcontcab_umb < lv_cantdif_tot.
*{   REPLACE        ER3K900279                                        3
*\        lv_p_confirm = 'NE'.
        lv_p_confirm = 'NE'.
        lv_p_confirm_lot = 'X'.
*}   REPLACE
      ELSEIF  lv_cantcontcab_umb = lv_cantdif_tot.
        lv_p_confirm = 'OK'.
      ENDIF.


*... Guardo la cantidad contada del dispositivo
      lv_cantcont_umb = les_reg_posiciones-cantcont.
      lv_cantcont_umd = lv_cantcont_umb.


***... Convierto la cantidad contada enviada del dispositivo y la convierto a UMD
**      READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_reg_posiciones-umc.
**      IF sy-subrc EQ 0.
**        lv_cantcont_umd = lv_cantcont_umb /  les_detalle-cantidad_ean.
**      ENDIF.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
      IF les_picking_dt-rsnum IS NOT INITIAL .
*... Si tiene reserva se toma la unidad de medida de la posición de picking
        READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_picking_dt-umc.
      ELSE.
*... Si no tiene reserva se toma la unidad de medida de la cabecera del dispositivo
        READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_reg_posiciones-umc.
      ENDIF.
      IF sy-subrc EQ 0.
        lv_cantcont_umd = lv_cantcont_umb /  les_detalle-cantidad_ean.
      ENDIF.

*
*    READ TABLE lti_picking_dt  INTO les_picking_dt with key
*    vbeln2 = les_reg_posiciones-vbeln2 material = les_reg_posiciones-material posicion = les_reg_posiciones-posicion
*    ubicacion_tmp = les_reg_posiciones-ubicacion_tmp ubicacion_fi = les_reg_posiciones-ubicacion_fi.
**... verificamos que al menos haya un registro con las condiciones
*    IF les_picking_dt is not initial.
      IF lv_cantcont_umd > 0 .
*... Recorro las posiciones del documento de picking con Unidad temporal  y Fija establecidas
        LOOP AT lti_picking_dt  INTO les_picking_dt
        WHERE material = l_e_matnr  AND "posicion = les_reg_posiciones-posicion AND "vbeln2 = les_reg_posiciones-vbeln2 AND
        diferencia > 0  AND ubicacion_tmp = les_reg_posiciones-ubicacion_tmp AND ubicacion_fi = les_reg_posiciones-ubicacion_fi.

*... Si la cantidad contada es menor que la cantidad disponible para la posicion del documento de picking asigno toda la cantidad contada
          IF lv_cantcont_umd <  les_picking_dt-diferencia AND lv_cantcont_umd > 0 .
            les_picking_dt-cantcont = les_picking_dt-cantcont + lv_cantcont_umd.
            les_picking_dt-umcc  = les_picking_dt-umc.
            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
            les_picking_dt-umd  = les_picking_dt-umc.
            les_picking_dt-conteo = les_reg_posiciones-conteo.
            les_picking_dt-usuario = i_usuario.
*          les_picking_dt-p_confirm = 'OK'.
            les_picking_dt-p_confirm = lv_p_confirm.
            lv_cantcont_umd = 0.
            MODIFY lti_picking_dt FROM les_picking_dt TRANSPORTING cantcont diferencia p_confirm conteo usuario WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick ."INDEX sy-tabix.
            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-umcc  = les_picking_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
*          MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.
            EXIT.
          ELSEIF lv_cantcont_umd > 0.
*... Si la cantidad contada es mayor o igual que la cantidad disponible para la posicion del documento de picking asigno lo que falta (Diferencia )
*... y Actualizo la cantidad contada
            lv_cantcont_umd = lv_cantcont_umd - les_picking_dt-diferencia.
            les_picking_dt-cantcont =  les_picking_dt-cantcont + les_picking_dt-diferencia.
            les_picking_dt-umcc  = les_picking_dt-umc.
            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
            les_picking_dt-umd  = les_picking_dt-umc.
            les_picking_dt-conteo = les_reg_posiciones-conteo.
            les_picking_dt-usuario = i_usuario.
*          les_picking_dt-p_confirm = 'OK'.
            les_picking_dt-p_confirm = lv_p_confirm.
            MODIFY lti_picking_dt FROM les_picking_dt TRANSPORTING cantcont diferencia umcc umd p_confirm conteo usuario WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick ."INDEX sy-tabix.

            MODIFY c_picking_det_sup FROM les_picking_dt TRANSPORTING cantcont diferencia  umcc umd p_confirm usuario WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick ."INDEX sy-tabix.

            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-umcc  = les_picking_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
          ENDIF.

        ENDLOOP.
      ENDIF.
*Si la cantidad contada es mayor a 0 , se necesita crear otro registro
      IF lv_cantcont_umd > 0 .
*... Recorro las posiciones del documento de picking con sin unidad temporal y fija
        LOOP AT lti_picking_dt  INTO les_picking_dt
        WHERE material = l_e_matnr  " AND posicion = les_reg_posiciones-posicion "vbeln2 = les_reg_posiciones-vbeln2 AND
          AND diferencia > 0 AND ubicacion_tmp IS INITIAL AND ubicacion_fi IS INITIAL .

*... Si la cantidad contada es menor que la cantidad disponible para la posicion del documento de picking
*... actualizo el registro quitandole la cantidad contada y creo otro registro con toda la cantidad contada
          IF lv_cantcont_umd <  les_picking_dt-diferencia AND lv_cantcont_umd > 0.

            les_picking_dt-cantidad = les_picking_dt-cantidad - lv_cantcont_umd.
            les_picking_dt-umcc  = les_picking_dt-umc.
            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
            les_picking_dt-umd  = les_picking_dt-umc.
            les_picking_dt-conteo = les_reg_posiciones-conteo.
            MODIFY lti_picking_dt FROM les_picking_dt TRANSPORTING cantidad diferencia cantcont umd umcc p_confirm conteo WHERE picknum = les_picking_dt-picknum AND
          pospick = les_picking_dt-pospick .
*            MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.

            MODIFY c_picking_det_sup FROM les_picking_dt TRANSPORTING cantidad diferencia cantcont umd umcc WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick ."INDEX sy-tabix.

            lv_consecutivo = lv_consecutivo + 1.
            les_picking_dt-pospick = lv_consecutivo.
            les_picking_dt-cantidad = lv_cantcont_umd.
            les_picking_dt-cantcont = lv_cantcont_umd.
            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
            les_picking_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
            les_picking_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
            les_picking_dt-usuario = i_usuario.
*            les_picking_dt-p_confirm = 'OK'.
            les_picking_dt-p_confirm = lv_p_confirm.
            APPEND les_picking_dt TO lti_picking_dt.
            APPEND les_picking_dt TO c_picking_det_sup.

            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-umcc  = les_picking_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
            lv_cantcont_umd = 0.
            EXIT.
          ELSEIF lv_cantcont_umd > 0.
*... Si la cantidad contada es mayor o igual que la cantidad disponible para la posicion del documento de picking asigno lo que falta (Diferencia )
*... y Actualizo la cantidad contada
*            les_picking_dt-cantidad =  les_picking_dt-cantidad - les_picking_dt-diferencia.
*            MODIFY lti_picking_dt from les_picking_dt index sy-tabix.
            lv_cantcont_umd = lv_cantcont_umd -  les_picking_dt-diferencia.
            les_picking_dt-cantcont =  les_picking_dt-cantcont + les_picking_dt-diferencia.
            les_picking_dt-umcc  = les_picking_dt-umc.
            les_picking_dt-diferencia = les_picking_dt-cantidad - les_picking_dt-cantcont.
            les_picking_dt-umd  = les_picking_dt-umc.
            les_picking_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
            les_picking_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
            les_picking_dt-conteo = les_reg_posiciones-conteo.
            les_picking_dt-usuario = i_usuario.
*            les_picking_dt-p_confirm = 'OK'.
            les_picking_dt-p_confirm = lv_p_confirm.
            MODIFY lti_picking_dt FROM les_picking_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi umd umcc p_confirm conteo usuario WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick .
*            MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.

            MODIFY c_picking_det_sup FROM les_picking_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi  umd umcc p_confirm usuario WHERE picknum = les_picking_dt-picknum AND
            pospick = les_picking_dt-pospick .

            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-umcc  = les_picking_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
*            APPEND les_picking_dt  to lti_picking_dt.

          ENDIF.

        ENDLOOP.

      ENDIF.


      IF lv_cantcont_umd = 0.
        LOOP AT lti_picking_dt INTO les_picking_dt.
          les_picking_dt-p_confirm = lv_p_confirm.
          MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.
        ENDLOOP.
        MODIFY  zmm_t_picking_dt FROM TABLE lti_picking_dt.
*... Guardo en las tablas auxiliares los valores por defecto
        lti_reg_posiciones  = c_reg_posiciones.
        lti_cab_reg_posiciones = c_cab_posiciones.
      ELSE.
        LOOP AT lti_picking_dt INTO les_picking_dt.
          les_picking_dt-p_confirm = 'NM'.
          MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.

          READ TABLE lti_reg_posiciones INTO les_reg_posiciones WITH KEY picknum = les_picking_dt-picknum
          material = les_picking_dt-material
          posicion = les_picking_dt-posicion.
          IF sy-subrc EQ 0 .
            les_reg_posiciones-p_confirm = 'NM'.
            MODIFY lti_reg_posiciones FROM les_reg_posiciones INDEX sy-tabix.
          ENDIF.

        ENDLOOP.
*        MODIFY  zmm_t_picking_dt FROM TABLE lti_picking_dt.
      ENDIF.
    ELSE.
*      IF le_flg_aglutinador is initial.
*    ....  Añado la novedad a cada registro del material
          LOOP AT lti_picking_dt INTO les_picking_dt.

            les_picking_dt-p_confirm = 'NM'.
            MODIFY lti_picking_dt FROM les_picking_dt INDEX sy-tabix.

            READ TABLE lti_reg_posiciones INTO les_reg_posiciones WITH KEY picknum = les_picking_dt-picknum
            material = les_picking_dt-material
            posicion = les_picking_dt-posicion.
            IF sy-subrc EQ 0 .
              les_reg_posiciones-p_confirm = 'NM'.
              MODIFY lti_reg_posiciones FROM les_reg_posiciones INDEX sy-tabix.
            ENDIF.

          ENDLOOP.
*    ....   Modifico la tabla Z con los NM
          MODIFY  zmm_t_picking_dt FROM TABLE lti_picking_dt.
*    .... Cantidad contada excedida
          l_e_cexc_error = lv_cantcontcab_umb - lv_cantdif_tot.

*    .... Guardo Mensaje
          les_msg-num_doc = i_documento_origen.
          CONCATENATE 'Material ' les_picking_dt-material 'Excede cantidad en ' l_e_cexc_error
          INTO l_e_desc_error SEPARATED BY space.
          les_msg-msg = l_e_desc_error.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.

*      ELSE.
**.... Si es un aglutinador y se realiza una adicion
*          lv_p_confirm = 'OK'.
*      ENDIF.

    ENDIF.
  ENDLOOP.
*.... Recorro los registros que son adiciones
  LOOP AT c_reg_posiciones INTO les_reg_posiciones where p_confirm EQ 'A'.
     lv_p_confirm = 'OK'.
     CLEAR:les_picking_dt ,lv_cantdif_tot,lv_cantcontcab_umb.
*... Copio la información de la tabla de posiciones del documento de picking a una tabla auxiliar
    lti_picking_dt = c_picking_det_sup.
*... consulto el registro de cabecera del dispositivo
    READ TABLE lti_picking_dt INTO les_picking_dt WITH KEY posicion  = les_reg_posiciones-posicion material = les_reg_posiciones-material
    ubicacion_tmp = les_reg_posiciones-ubicacion_tmp  aufnr = les_reg_posiciones-aufnr.
*... Si se encuentra algún registro
    IF sy-subrc eq 0 .
      lv_consecutivo = lv_consecutivo + 1.
      les_picking_dt-pospick = lv_consecutivo.
      les_picking_dt-cantidad =  les_reg_posiciones-cantcont .
      les_picking_dt-cantcont =  les_reg_posiciones-cantcont .
      les_picking_dt-umcc  = les_picking_dt-umc.
      les_picking_dt-diferencia = 0.
      les_picking_dt-umd  = les_picking_dt-umc.
      les_picking_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
      les_picking_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
      les_picking_dt-conteo = les_reg_posiciones-conteo.
      les_picking_dt-usuario = i_usuario.
      les_picking_dt-p_confirm = lv_p_confirm.
      APPEND les_picking_dt TO lti_picking_dt.
    ENDIF.
*    ....   Modifico la tabla Z adicionando cantidades
          MODIFY  zmm_t_picking_dt FROM TABLE lti_picking_dt.
  ENDLOOP.

*{   INSERT         ER3K900279                                        4
  clear: les_reg_posiciones.
  READ TABLE c_reg_posiciones INTO les_reg_posiciones WITH KEY conteo = '3'.

*  IF LV_P_CONFIRM_LOT NE 'X' or les_reg_posiciones is not INITIAL.
    CALL METHOD zcl_lgtica_picking=>add_ser_lot
    EXPORTING
      i_documento_origen = i_documento_origen
      i_entrega          = i_entrega
      i_tipo_documento   = i_tipo_documento
      i_clase_documento  = i_clase_documento
      i_usuario          = i_usuario
      c_mensajes         = c_mensajes
    CHANGING
      c_picking_ser      = c_picking_ser
      c_picking_lot      = c_picking_lot
      c_reg_posiciones   = c_reg_posiciones
      C_PICKING_DET_SUP   = C_PICKING_DET_SUP.
*  ENDIF.
*}   INSERT

ENDMETHOD.
ENDCLASS.
