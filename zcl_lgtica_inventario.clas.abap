class ZCL_LGTICA_INVENTARIO definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_INVENTARIO
*"* do not include other source files here!!!
public section.

  data NUMERO_DDOCUMENTO type IBLNR .
  data DETALLES_USUARIO type ZMM_T_IMP_ETIQ .
  data MENSAJES type ZTTSD_MSG_INVENTARIO .

  methods CONSTRUCTOR
    importing
      !I_USUARIO type ZED_USUARIO_MOVIL .
  methods GET_DETALLES_USUARIO
    importing
      !I_USUARIO type ZED_USUARIO_MOVIL
    exporting
      !E_DET_USUARIO type ZMM_T_IMP_ETIQ
    changing
      !C_MENSAJES type ZTTSD_MSG_DESPACHO .
  methods GET_HEADER_DATA
    importing
      !I_FEC_INI_BUS type ZE_FECHAS
      !I_FEC_FIN_BUS type ZE_FECHAS
      !I_DOCUMENTO type IBLNR
      !I_USUARIO type ZED_USUARIO_MOVIL
    exporting
      !E_CABECERA type ZTTSD_CAB_INVENTARIO
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  methods GET_DETAIL_DATA
    importing
      !I_DOCUMENTO type IBLNR
      !I_USUARIO type ZED_USUARIO_MOVIL
      !I_GJAHR type GJAHR optional
    exporting
      !E_DETALLE type ZTTMM_ISEG_UBICA
      !E_TI_OBJK type ZTT_OBJK
      !E_TI_INV_SER type ZMM_TT_INV_SER
      !E_CONTEO type ZED_CANT_CNT
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  class-methods INICIAR_CONTEO
    importing
      !I_DOCUMENTO type IBLNR
      !I_GJAHR type GJAHR
      !I_SERIE type ZMMTT_INVC_SERIE
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  class-methods MODIFICAR_CONTEO
    importing
      !I_DOCUMENTO type IBLNR
      !I_GJAHR type GJAHR
      !I_DETALLE type ZTTMM_ISEG_UBICA
      !I_SERIE type ZMMTT_INVC_SERPOS
      !I_TI_INV_SER type ZMM_TT_INV_SER
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  class-methods SET_DETAIL_NVD
    importing
      value(I_TI_INVENT_NV) type ZMM_TT_INVENT_D
      value(I_MOD) type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC
    changing
      value(C_MENSAJES) type ZTTSD_MSG_INVENTARIO .
  class-methods SET_HEADER_INV
    importing
      value(I_TI_INVENTARIO) type ZMM_TT_INVENT_H
      value(I_MOD) type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC
    changing
      value(C_MENSAJES) type ZTTSD_MSG_INVENTARIO .
  class-methods SET_DETAIL_INV
    importing
      value(I_TI_INVENT_PO) type ZMM_TT_INVENT_D
      value(I_MOD) type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC
    changing
      value(C_MENSAJES) type ZTTSD_MSG_INVENTARIO .
  methods CONTABILIZAR_INVENTARIO
    importing
      !I_DOCUMENTO type IBLNR
      !I_GJAHR type GJAHR
      !I_CONTABDOC type ZMMTT_CONTABDOC
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  methods CREAR_RECONTEO
    importing
      !I_DOCUMENTO type IBLNR
      !I_CONTEO type ZED_CANT_CNT
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO .
  methods LISTAR_DIFERENCIAS
    importing
      value(I_DOCUMENTO) type IBLNR
      value(I_USUARIO) type ZED_USUARIO_MOVIL
      value(I_DETALLE) type ZTTMM_ISEG_UBICA
    exporting
      value(E_TI_INVENT_PO) type ZMM_TT_INVENT_D
    changing
      value(C_MENSAJES) type ZTTSD_MSG_INVENTARIO .
  methods GET_SERIAL_DATA
    importing
      !I_DOCUMENTO type IBLNR
      !I_USUARIO type ZED_USUARIO_MOVIL
      !I_GJAHR type GJAHR
    exporting
      !E_TI_OBJK type ZTT_OBJK
      !E_TI_INV_SER type ZMM_TT_INV_SER
    changing
      !C_MENSAJES type ZTTSD_MSG_INVENTARIO optional .
  class-methods SET_SERIAL_INV
    importing
      value(I_TI_INV_SER) type ZMM_TT_INV_SER
      value(I_MOD) type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC
    changing
      value(C_MENSAJES) type ZTTSD_MSG_INVENTARIO .
protected section.
*"* protected components of class ZCL_LGTICA_INVENTARIO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_INVENTARIO
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_INVENTARIO IMPLEMENTATION.


method CONSTRUCTOR.

*.....Llamado a Metodo para Obtener los Detalles de Usuario
  CALL METHOD me->get_detalles_usuario
    EXPORTING
      i_usuario     = i_usuario
    IMPORTING
      e_det_usuario = detalles_usuario
    CHANGING
      c_mensajes    = mensajes.

endmethod.


METHOD contabilizar_inventario.

  DATA: lv_true       TYPE c VALUE 'X',
        lti_return    TYPE bbp_bapireturn_t,
        les_return    LIKE LINE OF lti_return,
        les_mensajes  LIKE LINE OF c_mensajes,
        lti_contabdoc TYPE zmmtt_contabdoc.

*  ASignar valores a tabla de contabilización.
  lti_contabdoc[] = i_contabdoc[].

*  Funcion de contabilización.
  CALL FUNCTION 'ZMF_CONTABDOC'
    EXPORTING
      i_iblnr     = i_documento
      i_gjahr     = i_gjahr
      i_date_rec  = sy-datum
      i_commit    = lv_true
    CHANGING
      t_contabdoc = lti_contabdoc
      t_return    = lti_return.

  LOOP AT lti_return INTO les_return.
*   TRaspasamos valores de tabla de mensajes de BAPI.
    les_mensajes-num_doc = i_documento.
    les_mensajes-msg = les_return-message.
    les_mensajes-type_msg = les_return-type.
    APPEND les_mensajes TO c_mensajes.
  ENDLOOP.


ENDMETHOD.


METHOD crear_reconteo.

  DATA: lv_iblnr       TYPE iblnr,
        lv_conteo      TYPE zed_cant_cnt,
        lti_inventario TYPE TABLE OF zmm_t_inventario,
        les_inventario TYPE zmm_t_inventario.

*  Completar ceros a la izquierda.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.

*  Se mapean los datos y se agregan a la tabla interna.
  les_inventario-iblnr = lv_iblnr.
  les_inventario-conteo = i_conteo.
  APPEND les_inventario TO lti_inventario.

*  Insertar registros en tabla de cabecera de inventario.
  CALL METHOD zcl_lgtica_inventario=>set_header_inv
    EXPORTING
      i_ti_inventario = lti_inventario
      i_mod           = 'I'
*    IMPORTING
*      e_dbcnt         =
*      e_subrc         =
    changing
      c_mensajes      = c_mensajes.

ENDMETHOD.


METHOD get_detail_data.

*  Estructura para el manejo de mensajes.
  DATA: les_mensajes TYPE zesd_msg_inventario,
        les_detalle  LIKE LINE OF e_detalle,
        lti_makt     TYPE TABLE OF makt,
        les_makt     TYPE makt,
        lv_iblnr     TYPE iblnr,
        lv_conteo    TYPE zed_cant_cnt.

  DATA: lti_return   TYPE TABLE OF bapiret1,
        les_return   LIKE LINE OF lti_return,
        lti_serno    TYPE TABLE OF zstmm_0003,
        les_serno    LIKE LINE OF lti_serno,
        lti_charg    TYPE TABLE OF zstmm_0002,
        les_charg    LIKE LINE OF lti_charg,
        lti_estatin  TYPE TABLE OF zstmm_0001,
        les_estatin  LIKE LINE OF lti_estatin,
        lti_matnr    TYPE TABLE OF zstmm_0004,
        les_matnr    LIKE LINE OF lti_matnr,
        lti_novedad  TYPE TABLE OF zmm_t_invent_nv,
        les_novedad  LIKE LINE OF lti_novedad.

*  Completar ceros a la izquierda.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.

*  Consultamos el detalle del inventario.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE e_detalle
    FROM zmm_t_iseg_ubica
    WHERE iblnr EQ lv_iblnr.

*   Obtener numero de conteo.
  SELECT SINGLE MAX( conteo )
    FROM zmm_t_inventario
    INTO lv_conteo
    WHERE iblnr EQ lv_iblnr.

  IF lv_conteo IS NOT INITIAL.

*  Obtener datos de tabla de novedades.
    SELECT *
      FROM zmm_t_invent_nv
      INTO TABLE lti_novedad
      WHERE iblnr = lv_iblnr
        AND conteo = lv_conteo.
*    Si se encuentran datos de novedades se mapean los datos para
*    agregarlos al detalle.
    IF sy-subrc EQ 0.
      LOOP AT lti_novedad INTO les_novedad.

*        Mapeo de datos.
        MOVE-CORRESPONDING les_novedad TO les_detalle .
*  Completar ceros a la izquierda.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = les_novedad-material
          IMPORTING
            output = les_detalle-matnr.
**        les_detalle-matnr     = les_novedad-material.
        les_detalle-ubicacion = les_novedad-ubicacion_fi.
        les_detalle-charg     = les_novedad-lote.
        les_detalle-menge     = les_novedad-cantcont.
        les_detalle-meins     = les_novedad-umcc.
        les_detalle-meins     = les_novedad-umcb.
        les_detalle-meins     = les_novedad-umd.
        APPEND les_detalle TO e_detalle.

      ENDLOOP.
    ENDIF.
*    Asiganr valor a variable de retorno de conteo.
    e_conteo = lv_conteo.

  ELSE.
*    En caso de no encontrar valor en tabla de inventario se
*    tomara como el primer conteo.
    e_conteo = 1.
  ENDIF.

*   Obtener descripción del material.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lti_makt
    FROM makt FOR ALL ENTRIES IN e_detalle
    WHERE matnr EQ e_detalle-matnr.

*  Agregar materiales a tabla de materiales para hallar los
*  materiales que tengan seriales.
  LOOP AT e_detalle INTO les_detalle.
*  Completar ceros a la izquierda.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = les_detalle-matnr
      IMPORTING
        output = les_matnr-matnr.
*    les_matnr-matnr = les_detalle-matnr.
    APPEND les_matnr TO lti_matnr.
  ENDLOOP.

*  Funcion para obtener seriales y lotes de los materiales.
  CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
    EXPORTING
      werk      = detalles_usuario-centro
      lgort     = detalles_usuario-almacen
      username  = i_usuario
    TABLES
      t_return  = lti_return
      t_serno   = lti_serno     "Serie para el Material
      t_charg   = lti_charg     "Lote para el Material
      t_estatin = lti_estatin
      t_matnr   = lti_matnr.

*Recorrer la tabla de detalle para mapear datos faltantes.
  LOOP AT e_detalle INTO les_detalle.

*   Llenar la descripción en la tabla de retorno.
    READ TABLE lti_makt INTO les_makt WITH KEY matnr = les_detalle-matnr.
    IF sy-subrc EQ 0.
      les_detalle-maktx = les_makt-maktx.
    ENDIF.

*    Convertir unidades de medida.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = les_detalle-meins
        language       = sy-langu
      IMPORTING
        output         = les_detalle-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF les_detalle-meins EQ '004'.
      les_detalle-meins = 'UN'.
    ENDIF.

*  Completar ceros a la izquierda.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = les_detalle-matnr
      IMPORTING
        output = les_detalle-matnr.
*    Agregar flag que indica si material tiene seriales o no.
    READ TABLE lti_serno INTO les_serno
      WITH KEY material = les_detalle-matnr.
    IF sy-subrc EQ 0.
      les_detalle-serie = 'X'.
    ENDIF.
    les_detalle-conteo = e_conteo.
*    Actualizar valores de tabla de detalle.
    MODIFY e_detalle FROM les_detalle.

  ENDLOOP.

*  Seriales de materiales ingresados
  CALL METHOD me->get_serial_data
    EXPORTING
      i_documento = i_documento
      i_usuario   = i_usuario
      i_gjahr     = i_gjahr
    IMPORTING
      e_ti_objk   = e_ti_objk
      e_ti_inv_ser = e_ti_inv_ser
    CHANGING
      c_mensajes  = c_mensajes.


  IF sy-subrc NE 0.
*    En caso de no encontrarse resultados se muestra mensaje de error.
    les_mensajes-num_doc = i_documento.
    les_mensajes-msg = 'No existe detalle para el documento de inventario'.
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

ENDMETHOD.


method GET_DETALLES_USUARIO.

*.....Función que retorna para Un Usuario el USUARIO,COD_IMPRESORA,CENTRO,ALMACEN
  CALL FUNCTION 'Z_SD_CONSUL_DAT_USER_MOV'
    EXPORTING
      usuario     = i_usuario
    IMPORTING
      usuario_det = e_det_usuario
    TABLES
      t_mensajes  = c_mensajes.

endmethod.


METHOD get_header_data.
*{   REPLACE        ER3K900269                                        1
*\
*\*  Rango de fechas.
*\  DATA: lr_fecha      TYPE RANGE OF datum,
*\        les_fecha     LIKE LINE OF lr_fecha,
*\*  Rando de documento.
*\        lr_documento  TYPE RANGE OF iblnr,
*\        les_documento LIKE LINE OF lr_documento,
*\*  Estructura de tabla de cabecera.
*\        lti_ikpf      TYPE TABLE OF ikpf,
*\        les_ikpf      TYPE ikpf,
*\*   Estructura de retorno de cabecera.
*\        les_cabecera  TYPE zesd_cab_inventario,
*\*  Estructura manejo de mensajes.
*\        les_mensajes  TYPE zesd_msg_inventario,
*\        lv_iblnr      TYPE iblnr.
*\
*\
*\  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\    EXPORTING
*\      input  = i_documento
*\    IMPORTING
*\      output = lv_iblnr.
*\
*\*  Llenar rango de documento.
*\  IF i_documento IS NOT INITIAL.
*\    les_documento-low = lv_iblnr.
*\    les_documento-sign = 'I'.
*\    les_documento-option = 'EQ'.
*\    APPEND les_documento TO lr_documento.
*\  ENDIF.
*\
*\*  Llenar rango de fechas.
*\  IF i_fec_ini_bus IS NOT INITIAL AND i_fec_fin_bus IS NOT INITIAL.
*\    les_fecha-low = i_fec_ini_bus.
*\    les_fecha-high = i_fec_fin_bus.
*\    les_fecha-sign = 'I'.
*\    les_fecha-option = 'BT'.
*\    APPEND les_fecha TO lr_fecha.
*\  ELSEIF i_fec_ini_bus IS NOT INITIAL.
*\    les_fecha-low = i_fec_ini_bus.
*\*    les_fecha-high = i_fec_fin_bus.
*\    les_fecha-sign = 'I'.
*\    les_fecha-option = 'EQ'.
*\    APPEND les_fecha TO lr_fecha.
*\  ENDIF.
*\
*\*  Consultar detalle de cabecera.
*\  SELECT *
*\    FROM ikpf
*\    INTO TABLE lti_ikpf
*\    WHERE iblnr IN lr_documento
*\      AND bldat IN lr_fecha
*\      AND werks EQ detalles_usuario-centro
*\      AND lgort EQ detalles_usuario-almacen
*\      AND DSTAT ne 'X'.
*\
*\  IF sy-subrc EQ 0.
*\*    Mapeado de datos de retorno de cabecera.
*\    LOOP AT lti_ikpf INTO les_ikpf.
*\      les_cabecera-documento = les_ikpf-iblnr.
*\      les_cabecera-fecha     = les_ikpf-bldat.
*\      APPEND les_cabecera TO e_cabecera.
*\    ENDLOOP.
*\  ELSE.
*\*    En caso de no encontrarse resultados se muestra mensaje de error.
*\    les_mensajes-num_doc = i_documento.
*\    les_mensajes-msg = 'Numero documento de inventario sin datos de cabecera'.
*\    les_mensajes-type_msg = 'E'.
*\    APPEND les_mensajes TO c_mensajes.
*\  ENDIF.
*\
*\ENDMETHOD.


  TYPES:BEGIN OF ltp_ikpf,
    iblnr type ikpf-iblnr,
    bldat type ikpf-bldat,
    dstat type ikpf-dstat,
    END OF ltp_ikpf.

*  Rango de fechas.
  DATA: lr_fecha      TYPE RANGE OF datum,
        les_fecha     LIKE LINE OF lr_fecha,
*  Rando de documento.
        lr_documento  TYPE RANGE OF iblnr,
        les_documento LIKE LINE OF lr_documento,
*  Estructura de tabla de cabecera.
        lti_ikpf      TYPE TABLE OF ltp_ikpf,
        les_ikpf      TYPE ltp_ikpf,
*   Estructura de retorno de cabecera.
        les_cabecera  TYPE zesd_cab_inventario,
*  Estructura manejo de mensajes.
        les_mensajes  TYPE zesd_msg_inventario,
        lv_iblnr      TYPE iblnr.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.
***CO inicio: Hallazgo IBM
***  Llenar rango de documento.
**  IF i_documento IS NOT INITIAL.
**    les_documento-low = lv_iblnr.
**    les_documento-sign = 'I'.
**    les_documento-option = 'EQ'.
**    APPEND les_documento TO lr_documento.
**  ENDIF.
***CO fin: Hallazgo IBM
*  Llenar rango de fechas.
  IF i_fec_ini_bus IS NOT INITIAL AND i_fec_fin_bus IS NOT INITIAL.
    les_fecha-low = i_fec_ini_bus.
    les_fecha-high = i_fec_fin_bus.
    les_fecha-sign = 'I'.
    les_fecha-option = 'BT'.
    APPEND les_fecha TO lr_fecha.
  ELSEIF i_fec_ini_bus IS NOT INITIAL.
    les_fecha-low = i_fec_ini_bus.
*    les_fecha-high = i_fec_fin_bus.
    les_fecha-sign = 'I'.
    les_fecha-option = 'EQ'.
    APPEND les_fecha TO lr_fecha.
  ENDIF.
***MO Inicio: hallazgo IBM
***  Consultar detalle de cabecera.
**  SELECT *
**    FROM ikpf
**    INTO TABLE lti_ikpf
**    WHERE iblnr IN lr_documento
**      AND bldat IN lr_fecha
**      AND werks EQ detalles_usuario-centro
**      AND lgort EQ detalles_usuario-almacen
**      AND DSTAT ne 'X'.
*  Consultar detalle de cabecera.
  SELECT iblnr bldat dstat
    FROM ikpf
    INTO TABLE lti_ikpf
    WHERE iblnr EQ lv_iblnr
      AND bldat IN lr_fecha
      AND werks EQ detalles_usuario-centro
      AND lgort EQ detalles_usuario-almacen.

 DELETE lti_ikpf where dstat eq 'X'.
***MO Inicio: hallazgo IBM
  IF lti_ikpf is not INITIAL.
*    Mapeado de datos de retorno de cabecera.
    LOOP AT lti_ikpf INTO les_ikpf.
      les_cabecera-documento = les_ikpf-iblnr.
      les_cabecera-fecha     = les_ikpf-bldat.
      APPEND les_cabecera TO e_cabecera.
    ENDLOOP.
  ELSE.
*    En caso de no encontrarse resultados se muestra mensaje de error.
    MESSAGE E000(zclinv)  INTO les_mensajes-msg.
    les_mensajes-num_doc = i_documento.
*    les_mensajes-msg = 'Numero documento de inventario sin datos de cabecera'.
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

ENDMETHOD.
*}   REPLACE


METHOD get_serial_data.

  DATA: lti_ser07 TYPE TABLE OF ser07,
        les_ser07 LIKE LINE OF lti_ser07,
        lv_iblnr  TYPE iblnr.

  DATA: lti_inv_ser TYPE TABLE OF zmm_t_inv_ser,
        les_inv_ser LIKE LINE OF lti_inv_ser.

  FIELD-SYMBOLS: <fs_objk> TYPE LINE OF ztt_objk.
*  Completar ceros a la izquierda.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.

*  Obtener datos de seriales.
  SELECT *
    FROM ser07
    INTO TABLE lti_ser07
      WHERE iblnr = lv_iblnr
        AND mjahr = i_gjahr
        AND werk = detalles_usuario-centro
        AND lagerort = detalles_usuario-almacen.

  IF sy-subrc EQ 0.

    SELECT *
      FROM objk
      INTO TABLE e_ti_objk
      FOR ALL ENTRIES IN lti_ser07
      WHERE obknr = lti_ser07-obknr.
*      SORT lti_ser07 by OBKNR DESCENDING.
*      READ TABLE lti_ser07 into les_ser07 INDEX 1.
*
*      SORT e_ti_objk by OBKNR DESCENDING.
*      DELETE e_ti_objk WHERE OBKNR <> les_ser07-OBKNR.

  ENDIF.

  SELECT *
   FROM zmm_t_inv_ser
    INTO TABLE e_ti_inv_ser
      WHERE iblnr = i_documento
        .
ENDMETHOD.


METHOD iniciar_conteo.

*  Tabbla de reconteo.
  DATA:  lti_invc_recon  TYPE zmmtt_invc_recon,
         les_invc_recon  LIKE LINE OF lti_invc_recon,
*   Tabla de modificación.
         lti_invc_modif  TYPE zmmtt_invc_recon,
         lti_invc_serie  TYPE zmmtt_invc_serie,
*   Tabla de retorno de mensajes.
         lti_return      TYPE bbp_bapireturn_t,
         les_return      LIKE LINE OF lti_return,
         lv_true         TYPE c VALUE 'X',
         les_mensajes    LIKE LINE OF c_mensajes,
         lv_iblnr      TYPE iblnr.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.


*  Obtenemos todos los materiales y sus lotes del documento
*  ingresado.
  SELECT *
    FROM zmm_t_iseg_ubica
    INTO CORRESPONDING FIELDS OF TABLE lti_invc_recon
    WHERE iblnr = lv_iblnr
      AND gjahr = i_gjahr.

*  En caso de no encontrarse resultados se muestra error.
  IF sy-subrc NE 0.
    les_mensajes-num_doc = i_documento.
    les_mensajes-msg = 'Documento de inventario ingresado no existe'.
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ELSE.
*  Modificar flag que permite modificar los registros.
    LOOP AT lti_invc_recon INTO les_invc_recon.
      les_invc_recon-xnull = 'X'.
      MODIFY lti_invc_recon FROM les_invc_recon.
    ENDLOOP.

*    Asignar tabla de recuento a tabla de modificación.
    lti_invc_modif[] = lti_invc_recon[].
    lti_invc_serie[] = i_serie[].

*    Inicializar conteo.
    CALL FUNCTION 'ZMF_RECON_CREAR'
      EXPORTING
        i_iblnr        = lv_iblnr
        i_gjahr        = i_gjahr
        i_date_rec     = sy-datum
        i_commit       = lv_true
      CHANGING
        t_mat_recuento = lti_invc_recon
        t_mat_modify   = lti_invc_modif
        t_mat_serial   = lti_invc_serie
        t_return       = lti_return.

    LOOP AT lti_return INTO les_return.
*      En caso de no encontrarse resultados se muestra mensaje de error.
      les_mensajes-num_doc = i_documento.
      les_mensajes-msg = les_return-message.
      les_mensajes-type_msg = les_return-type.
      APPEND les_mensajes TO c_mensajes.
    ENDLOOP.
  ENDIF.


ENDMETHOD.


METHOD listar_diferencias.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Lista las diferencias en un documento de inventario
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  DATA:
*   Variable para manipular strings
        lv_msg TYPE string,
*  Estructura manejo de mensajes.
          les_mensajes  TYPE zesd_msg_inventario.
*... Estructuras para el manejo del detalle de documento de inventario
  DATA: lti_detalle TYPE zttmm_iseg_ubica,
        les_detalle TYPE zesd_det_inventario.
*.... Estructura para el manejo de las diferencias
  DATA: lti_novedad TYPE STANDARD TABLE OF zmm_t_invent_po,
        les_novedad TYPE zmm_t_invent_po.


data: E_TI_OBJK TYPE ZTT_OBJK.
DATA: e_ti_inv_ser type ZMM_TT_INV_SER.
DATA: e_conteo type ZED_CANT_CNT.




*... Valido que hayan ingresado un detalle de inventario
  IF i_detalle IS INITIAL AND i_documento IS NOT INITIAL.
*... Consulto el detalle de inventario si no se ingreso con el respectivo documento
    CALL METHOD me->get_detail_data
      EXPORTING
        i_documento = i_documento
        i_usuario   = i_usuario
      IMPORTING
        e_detalle   = i_detalle
        E_TI_OBJK = E_TI_OBJK
        E_TI_INV_SER = E_TI_INV_SER
        E_CONTEO =  E_CONTEO
      CHANGING
        c_mensajes  = c_mensajes.
*Elimino los registros con novedad
      DELETE i_detalle where zzeili is initial.
  ENDIF.
  CLEAR: c_mensajes.
*... Ordeno por material y ubicación
  SORT i_detalle BY matnr ubicacion.

*... Recorro el detalle y realizo la acumulación de las diferencias
  LOOP AT i_detalle INTO les_detalle where charg is initial.

    AT NEW  matnr.
      CLEAR:les_novedad.
      les_novedad-material = les_detalle-matnr.
      les_novedad-IBLNR = les_detalle-iblnr.

    ENDAT.
    les_novedad-CONTEO = les_detalle-conteo.
*... Acumulo la cantidad
    les_novedad-cantcont = les_novedad-cantcont + les_detalle-menge.
    les_novedad-UMCC = les_detalle-meins.
    les_novedad-buchm = les_detalle-zbuchm.
    les_novedad-UMCB = les_detalle-MEINS.
    les_novedad-UBICACION_FI = les_detalle-UBICACION.
    les_novedad-lote = les_detalle-charg.
*... Genero el registro si se encontro alguna novedad
    AT END OF ubicacion.

      les_novedad-diferencia = les_novedad-cantcont - les_novedad-buchm.
      les_novedad-UMD = les_detalle-MEINS.
      les_novedad-UBICACION_FI = les_detalle-UBICACION.
      IF les_novedad-diferencia NE 0.
        lv_msg = les_novedad-diferencia.
*    En caso de no encontrarse novedades.
        les_mensajes-num_doc = i_documento.
        CONCATENATE 'Material' les_novedad-material 'presenta una diferencia de' lv_msg INTO les_mensajes-msg SEPARATED BY space.

*    les_mensajes-msg = 'Material 'documento de inventario sin datos de cabecera'.
        les_mensajes-type_msg = 'E'.
        APPEND les_mensajes TO c_mensajes.
        APPEND les_novedad TO lti_novedad.
      ENDIF.
      les_novedad-cantcont = 0.
    ENDAT.

  ENDLOOP.


*... Ordeno por material y ubicación
  SORT i_detalle BY matnr charg.
  DELETE i_detalle where charg is initial.
*... Recorro el detalle y realizo la acumulación de las diferencias
  LOOP AT i_detalle INTO les_detalle where charg ne ''.

    AT NEW  matnr.
      CLEAR:les_novedad.
      les_novedad-material = les_detalle-matnr.
      les_novedad-IBLNR = les_detalle-iblnr.

    ENDAT.
    les_novedad-CONTEO = les_detalle-conteo.
*... Acumulo la cantidad

    les_novedad-cantcont = les_novedad-cantcont + les_detalle-menge.
    les_novedad-UMCC = les_detalle-meins.
    les_novedad-buchm = les_detalle-buchm.
    les_novedad-UMCB = les_detalle-MEINS.
    les_novedad-lote = les_detalle-charg.
    les_novedad-UBICACION_FI = les_detalle-UBICACION.
*... Genero el registro si se encontro alguna novedad
    AT END OF charg.

      les_novedad-diferencia = les_novedad-cantcont - les_novedad-buchm.
      les_novedad-UMD = les_detalle-MEINS.
*      les_novedad-UBICACION_FI = les_detalle-UBICACION.
      IF les_novedad-diferencia NE 0.
        lv_msg = les_novedad-diferencia.
*    En caso de no encontrarse novedades.
        les_mensajes-num_doc = i_documento.
        CONCATENATE 'Material' les_novedad-material 'presenta una diferencia de' lv_msg INTO les_mensajes-msg SEPARATED BY space.

*    les_mensajes-msg = 'Material 'documento de inventario sin datos de cabecera'.
        les_mensajes-type_msg = 'E'.
        APPEND les_mensajes TO c_mensajes.
        APPEND les_novedad TO lti_novedad.
      ENDIF.
      les_novedad-cantcont = 0.
    ENDAT.

  ENDLOOP.


  e_ti_invent_po = lti_novedad.
ENDMETHOD.


METHOD modificar_conteo.

  DATA: lv_true       TYPE c VALUE 'X',
        lv_iblnr      TYPE iblnr,
        lti_recon     TYPE zmmtt_invc_recon,
        les_recon     LIKE LINE OF lti_recon,
        lti_modify    TYPE zmmtt_invc_recon,
        les_modify    LIKE LINE OF lti_modify,
        lti_return    TYPE bbp_bapireturn_t,
        les_return    LIKE LINE OF lti_return,
        les_detalle   LIKE LINE OF i_detalle,
        lti_serie_2     TYPE zmmtt_invc_serie,
        les_serie_2 LIKE LINE OF lti_serie_2,

        les_mensajes  LIKE LINE OF c_mensajes,
        lti_serie     TYPE zmmtt_invc_serpos,
        les_serie LIKE LINE OF lti_serie.
*.... Variables Contador de seriales
DATA: lv_item type DZEILE,
      lv_zeili type DZEILE,
      lv_zzeili type DZEILE.
*.... Estructuras para el manejo de los datos de documento de inventario
  DATA: lti_iseg_ubica TYPE TABLE OF zmm_t_iseg_ubica,
        les_iseg_ubica LIKE LINE OF lti_iseg_ubica.

*  Completar ceros a la izquierda.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = lv_iblnr.


*  Consultamos el detalle del inventario.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lti_iseg_ubica
    FROM zmm_t_iseg_ubica
    WHERE iblnr EQ lv_iblnr.


*  Recorrer datos modificados e ingresarlos a la tabla interna.
  LOOP AT i_detalle INTO les_detalle.
*    Valido que el material se ha modificado con el flag de serie
    IF les_detalle-serie EQ 'X'.
      MOVE-CORRESPONDING les_detalle TO les_modify.
      les_modify-xzael = 'X'.
*  Completar ceros a la izquierda.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = les_modify-matnr
        IMPORTING
          output = les_modify-matnr.
      IF les_modify-menge = 0 .
        les_modify-xnull = 'X'.
      ENDIF.
     CALL FUNCTION 'ROUND'
        EXPORTING
          decimals             = 3
          input                = les_detalle-menge
          sign                 = '-'
        IMPORTING
          output               = les_modify-menge.

*      les_modify-menge = les_detalle-menge.

*    Convertir unidades de medida.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = les_detalle-meins
          language       = sy-langu
        IMPORTING
          output         = les_modify-meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
*.... Busco las cantidades teoricas de lote y ubicación
      READ TABLE lti_iseg_ubica INTO les_iseg_ubica WITH KEY iblnr = les_detalle-iblnr gjahr = les_detalle-gjahr zeili = les_detalle-zeili
      zzeili = les_detalle-zzeili ubicacion = les_detalle-ubicacion.
      IF sy-subrc EQ 0.
        les_modify-charg = les_iseg_ubica-charg.
        les_modify-buchm = les_iseg_ubica-buchm.
*{   INSERT         ER3K900329                                        1
        LES_MODIFY-STOCK_TYPE = les_iseg_ubica-STOCK_TYPE.
*}   INSERT
        les_modify-zbuchm = les_iseg_ubica-zbuchm.
      ENDIF.


      les_modify-erfmg = les_modify-menge.
      les_modify-erfme = les_modify-meins.
*     les_modify-meins = les_detalle-meins.
      APPEND les_modify TO lti_modify.

    ENDIF.
*.... Realizo operaciones para los datos de reconteo
    MOVE-CORRESPONDING les_detalle TO les_recon.
    les_recon-xzael = 'X'.
    IF les_detalle-menge = 0 .
      les_recon-xnull = 'X'.
    ENDIF.
    CALL FUNCTION 'ROUND'
        EXPORTING
          decimals             = 3
          input                = les_detalle-menge
          sign                 = '-'
        IMPORTING
          output               = les_recon-menge.
*    les_recon-menge = les_detalle-menge.
*    Convertir unidades de medida.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = les_detalle-meins
        language       = sy-langu
      IMPORTING
        output         = les_recon-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = les_recon-matnr
      IMPORTING
        output = les_recon-matnr.

    CALL FUNCTION 'ROUND'
        EXPORTING
          decimals             = 3
          input                = les_detalle-menge
          sign                 = '-'
        IMPORTING
          output               = les_recon-erfmg.
*    les_recon-erfmg = les_detalle-menge.
    les_recon-erfme = les_recon-meins.
*.... Busco las cantidades teoricas de lote y ubicación
    READ TABLE lti_iseg_ubica INTO les_iseg_ubica WITH KEY iblnr = les_detalle-iblnr gjahr = les_detalle-gjahr zeili = les_detalle-zeili
    zzeili = les_detalle-zzeili ubicacion = les_detalle-ubicacion.
    IF sy-subrc EQ 0.
      les_recon-charg = les_iseg_ubica-charg.
      les_recon-buchm = les_iseg_ubica-buchm.
      les_recon-zbuchm = les_iseg_ubica-zbuchm.
    ENDIF.

    APPEND les_recon TO lti_recon.

  ENDLOOP.



**  Obtener todos los lotes que se modificaran.
*  LOOP AT lti_modify INTO les_modify.
*    LOOP AT lti_iseg_ubica INTO les_iseg_ubica
*      WHERE matnr eq les_modify-matnr
*        AND charg EQ les_modify-charg
*        AND ubicacion NE les_modify-ubicacion.
*
*        MOVE-CORRESPONDING les_iseg_ubica TO les_recon.
**  Completar ceros a la izquierda.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = les_recon-matnr
*    IMPORTING
*      output = les_recon-matnr.
*        les_recon-ERFMG = les_modify-menge.
*        les_recon-ERFME = les_modify-meins.
*
*        APPEND les_recon TO lti_recon.
*    ENDLOOP.
*
*    MOVE-CORRESPONDING les_modify TO les_recon.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = les_recon-matnr
*    IMPORTING
*      output = les_recon-matnr.
*    les_recon-ERFMG = les_modify-menge.
*    les_recon-ERFME = les_modify-meins.
*    APPEND les_recon TO lti_recon.
*  ENDLOOP.
*
*
*  DELETE ADJACENT DUPLICATES FROM lti_recon COMPARING zeili zzeili.

  lti_serie[] = i_serie[].
  DELETE lti_serie WHERE zeili IS INITIAL.
  "  Actualizar it seriales para bapi
  IF lti_serie IS NOT INITIAL.
    READ TABLE lti_serie into les_serie index 1.
    lv_zeili = les_serie-zeili.
    lv_zzeili = les_serie-zzeili.

    LOOP AT lti_serie INTO les_serie.

      IF lv_zeili = les_serie-zeili and lv_zzeili = les_serie-zzeili.
        lv_item = lv_item + 1.
      ELSE.
        lv_item = 1.
        lv_zeili = les_serie-zeili.
        lv_zzeili = les_serie-zzeili.
      ENDIF.
      les_serie-item = lv_item.
      MODIFY lti_serie from les_serie INDEX sy-tabix.
      CLEAR: les_serie_2.
      les_serie_2-item         = les_serie-zeili.
      les_serie_2-serialno     = les_serie-serie.

      APPEND les_serie_2 TO lti_serie_2.

    ENDLOOP.
  ENDIF.

  SORT lti_recon by  menge DESCENDING.
  SORT lti_modify by zeili zzeili menge ASCENDING.
  SORT lti_serie by zeili zzeili item  ASCENDING.
  SORT lti_serie_2 by item  ASCENDING.

  CALL FUNCTION 'ZMF_RECON_MODIFICAR'
    EXPORTING
      i_iblnr        = lv_iblnr
      i_gjahr        = i_gjahr
      i_date_rec     = sy-datum
      t_mat_serpos   = lti_serie
      i_commit       = lv_true
    CHANGING
      t_mat_recuento = lti_recon
      t_mat_modify   = lti_modify
      t_mat_serial   = lti_serie_2
      t_return       = lti_return.


  READ TABLE lti_return INTO les_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL METHOD zcl_lgtica_inventario=>set_serial_inv
      EXPORTING
        i_ti_inv_ser = i_ti_inv_ser
        i_mod        = 'M'
*      IMPORTING
*        e_dbcnt      =
*        e_subrc      =
      CHANGING
        c_mensajes   = c_mensajes
        .
  ENDIF.
  LOOP AT lti_return INTO les_return.
*     En caso de no encontrarse resultados se muestra mensaje de error.
    les_mensajes-num_doc = i_documento.
    les_mensajes-msg = les_return-message.
    les_mensajes-type_msg = les_return-type.
    APPEND les_mensajes TO c_mensajes.
  ENDLOOP.

ENDMETHOD.


method SET_DETAIL_INV.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para ingresar registros a la tabla de inventario detalle novedades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_invent_po FROM TABLE I_TI_INVENT_po.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_invent_po FROM TABLE I_TI_INVENT_po.
    WHEN 'D'. " Borrar
      DELETE zmm_t_invent_po FROM TABLE I_TI_INVENT_po.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_invent_po FROM TABLE I_TI_INVENT_po.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
endmethod.


METHOD set_detail_nvd.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para ingresar registros a la tabla de inventario detalle novedades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  DATA: les_ti_invent_nv LIKE LINE OF i_ti_invent_nv.

  LOOP AT i_ti_invent_nv INTO les_ti_invent_nv.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = les_ti_invent_nv-iblnr
      IMPORTING
        output = les_ti_invent_nv-iblnr.
    MODIFY i_ti_invent_nv from  les_ti_invent_nv INDEX sy-tabix.
  ENDLOOP.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_invent_nv FROM TABLE i_ti_invent_nv.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_invent_nv FROM TABLE i_ti_invent_nv.
    WHEN 'D'. " Borrar
      DELETE zmm_t_invent_nv FROM TABLE i_ti_invent_nv.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_invent_nv FROM TABLE i_ti_invent_nv.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
ENDMETHOD.


method SET_HEADER_INV.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para ingresar registros a la tabla de inventario novedades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_inventario FROM TABLE I_TI_INVENTARIO.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_inventario FROM TABLE I_TI_INVENTARIO.
    WHEN 'D'. " Borrar
      DELETE zmm_t_inventario FROM TABLE I_TI_INVENTARIO.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_inventario FROM TABLE I_TI_INVENTARIO.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
endmethod.


method SET_SERIAL_INV.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para ingresar registros a la tabla de inventario detalle novedades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*


  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_inv_ser FROM TABLE I_TI_INV_SER.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_inv_ser FROM TABLE I_TI_INV_SER.
    WHEN 'D'. " Borrar
      DELETE zmm_t_inv_ser FROM TABLE I_TI_INV_SER.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_inv_ser FROM TABLE I_TI_INV_SER.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
endmethod.
ENDCLASS.
