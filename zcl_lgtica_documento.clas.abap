class ZCL_LGTICA_DOCUMENTO definition
  public
  abstract
  create public .

*"* public components of class ZCL_LGTICA_DOCUMENTO
*"* do not include other source files here!!!
public section.

  interfaces ZIF_LGTICA_DOCUMENTO .
  interfaces ZIF_LGTICA_PACKING .
  interfaces ZIF_LGTICA_PICKABLE .

  aliases GET_TIPO_DOCUMENTO
    for ZIF_LGTICA_DOCUMENTO~GET_TIPO_DOCUMENTO .

  data CABECERAS_VENTAS type ZTTSD_CABECERA_VENTAS .
  data DETALLES_VENTAS type ZTTSD_POSICIONES_VENTAS .
  data CABECERAS_COMPRAS type ZTTSD_CABECERA_COMPRAS .
  data DETALLES_COMPRAS type ZTTSD_POSICIONES_COMPRAS .
  data CABECERAS_ORDENES type ZESD_CABECERA_ORDENES .
  data CABECERA_RESERVAS type ZESD_CABECERA_RESERVAS .
  data DETALLES_RESERVAS type ZTTSD_DETALLE_RESERVAS .
  data DETALLES_USUARIO type ZMM_T_IMP_ETIQ .
  data CABECERA_AGLUTINADOR type ZTTSD_CABECERA_AGLUT .
  data MENSAJES type ZTTSD_MSG_PICKING .

  methods CONSTRUCTOR
    importing
      value(I_TIPO_DOC) type ZE_TIPO_DOC
      value(I_USUARIO) type ZED_USUARIO_MOVIL .
  methods GET_DETALLES_USUARIO
    importing
      value(I_USUARIO) type ZED_USUARIO_MOVIL optional
    exporting
      value(E_DET_USUARIO) type ZMM_T_IMP_ETIQ
    changing
      value(C_MENSAJES) type ZTTSD_MSG_PICKING optional .
  methods GET_AJUSTAR_DOCUMENTO
    importing
      !I_DOCUMENTO type ZE_DOCUMENTO
    returning
      value(R_DOCUMENTO) type ZE_DOCUMENTO .
  methods CALL_BAPI_DELIVERY1
    importing
      value(I_LEDAT) type LFDAT_A
      value(I_NUR_VORGABE_POS) type XFELD
      value(I_KEY_ENQUE_READ) type SHP_VL10_PACKAGE_T
      value(I_VETVG) type SHP_VL10_VETVG_T optional
    exporting
      value(E_VBLS) type SHP_VBLS_T
      value(E_VBSK) type SHP_VBSK_T
      value(E_SYST) type SYST .
  methods CALL_BAPI_DELIVERY2
    importing
      value(I_VBSK_I) type VBSK
      value(I_XKOMDLGN) type ZTTSD_KOMDLGN
    exporting
      value(E_SYST) type SYST
    changing
      value(LTI_VBFS) type ZTTSD_VBFS
    returning
      value(R_XLIPS) type ZTTSD_LIPS .
  methods SET_CAB_CANT1
    importing
      value(PROCESO) type ZED_PROCESO optional
      value(SUBPROCESO) type ZED_PROCESO optional
      value(ESTADO) type ZED_ESTADO_PICKING optional
      value(ENTREGA) type VBELN_VL
      value(USUARIO) type ZED_USUARIO_MOVIL
      value(VBELN) type VBELN_VA .
  methods SET_CAB_CANT2
    importing
      value(PROCESO) type ZED_PROCESO optional
      value(SUBPROCESO) type ZED_PROCESO optional
      value(ESTADO) type ZED_ESTADO_PICKING optional
      value(ENTREGA) type VBELN_VL
      value(USUARIO) type ZED_USUARIO_MOVIL
      value(EBELN) type EBELN .
  methods SET_CAB_CANT3
    importing
      value(PROCESO) type ZED_PROCESO optional
      value(SUBPROCESO) type ZED_PROCESO optional
      value(ESTADO) type ZED_ESTADO_PICKING optional
      value(ENTREGA) type VBELN_VL
      value(USUARIO) type ZED_USUARIO_MOVIL
      value(AGLUTINADOR) type ZED_AGLUTINADOR .
  methods GET_PICKNUM
    importing
      value(ENTREGA) type VBELN_VL optional
      value(USUARIO) type ZED_USUARIO_MOVIL optional
    exporting
      value(PICKNUM) type ZED_PICKNUM .
  methods CREATE_TO_FOR_DELIVERY
    importing
      value(I_LGNUM) type LGNUM
      value(I_ENTREGA) type VBELN_VL
    exporting
      value(E_TANUM) type TANUM
      value(E_BAPIRET1) type BAPIRET1 .
  methods POST_GOODS_ISSUE
    importing
      value(I_ENTREGA) type VBELN_VL
    changing
      value(C_BAPIRET1) type BAPIRET1_TAB .
  methods CREAR_TRANSPORTE
    importing
      value(I_ENTREGA) type ZESD_DET_ENT_PKG
    exporting
      value(E_TRANSPORTE) type TANUM
    changing
      value(C_RETURN) type ZTTSD_MSG_PICKING .
  methods CONTABILIZA_ENTREGA
    importing
      value(I_ENTREGA) type ZESD_DET_ENT_PKG
    changing
      value(C_RETURN) type ZTTSD_MSG_PICKING .
  methods ACTUALIZA_ENTREGA
    importing
      value(I_ENTREGA) type ZESD_DET_ENT_PKG
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
    changing
      value(C_RETURN) type ZTTSD_MSG_PICKING .
  methods CONTABILIZA_ORDEN
    importing
      value(I_ENTREGA) type ZESD_DET_ENT_PKG optional
    changing
      value(C_RETURN) type ZTTSD_MSG_PICKING .
  methods CONTABILIZA_AGLUTINADOR
    changing
      value(C_RETURN) type ZTTSD_MSG_PICKING .
protected section.
*"* protected components of class ZCL_LGTICA_DOCUMENTO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_DOCUMENTO
*"* do not include other source files here!!!

  methods ADD_CANTIDADES1
    importing
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO
      value(I_ENTREGA) type VBELN_VL
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO
      value(I_USUARIO) type ZED_USUARIO_MOVIL
      !C_REG_POSICIONES type ZTTSD_PICKING_DET
    changing
      !C_MENSAJES type ZTTSD_MSG_PICKING .
ENDCLASS.



CLASS ZCL_LGTICA_DOCUMENTO IMPLEMENTATION.


METHOD actualiza_entrega.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado de Actualizar Entregas
*
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 21.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 21.07.2014    ER6K907097    Marco A. Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
*..... Cantidad contada por posición de picking
        l_e_cant_cont TYPE zedsd_cant_cnt        ,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum,
*.....Número material
       l_e_nmat    TYPE matnr, " SO-128 LFY 07/04/2015
*.....Control Llenado de Estructura
        l_e_control TYPE i,
*.....Control para la Cantidad de la Posicion
        l_e_cont_cant TYPE i,
*.....Control para la Cantidad de seriales
        l_e_cont_cant_ser TYPE i,
*.....Variable de Ref a la Clase Picknum
        lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Información Detalles de Documento Picking
*{   INSERT         ER3K900309                                       31
        lti_picking_det_x TYPE zttsd_picking_det,
        lvs type integer,
*}   INSERT
        lti_picking_det TYPE zttsd_picking_det,
        lti_picking_det_ubic TYPE zttsd_picking_det,
*        les_picking_det TYPE zedsd_picking_det,
*.....Información Seriales
        lti_picking_ser TYPE zttsd_picking_ser,
        les_picking_ser TYPE zedsd_picking_ser,
*.....Información Lotes
        lti_picking_lotes TYPE zttsd_picking_lot,
        les_picking_lotes TYPE zedsd_picking_lot,
*.....Tablas Auxiliares para Tablas y Lotes
        lti_picking_lotes_aux TYPE zttsd_picking_lot,
        lti_picking_ser_aux TYPE zttsd_picking_ser,
*.....Estructuras de Cabeceras
*{   REPLACE        ER3K900309                                       10
*\        les_cab_ventas TYPE vbak,
        les_cab_ventas TYPE ZESD_CAB_PED,
*}   REPLACE
*{   REPLACE        ER3K900309                                       11
*\        les_cab_compras TYPE ekko,
        les_cab_compras TYPE ZESD_CAB_CMP,
*}   REPLACE
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.... Estructuras de detalles
        les_det_ventas TYPE zesd_posiciones_ventas,
        les_det_compras TYPE zesd_posiciones_compras,
*.... Estructura de ubicaciones
        les_det_ubicacion TYPE zmm_t_ubicacion,
*.....Cantidad Contada para cada Posicion
        l_e_cant_pos TYPE zedsd_cant_cnt,
*.....Estructuras y Tablas para Funcion que Actualiza la Entrega
        les_vbkok TYPE vbkok,
        lti_sernr_update TYPE shp_sernr_update_t,
        les_sernr_update TYPE shp_sernr_update_s,
        lti_vbpok TYPE tab_vbpok,
        les_vbpok TYPE vbpok,
        les_vbpok_lote TYPE vbpok,
        les_vbpok_eli TYPE vbpok,
        l_e_error_any TYPE xfeld,
*{   INSERT         ER3K900309                                        1
         les_entrega type zesd_det_ent_pkg,
         lti_vbpok_aux TYPE tab_vbpok,
         les_flag_tras_del type c,
         I_MATNR type  MARA-MATNR,
         I_IN_ME type  MARA-MEINS,
         I_OUT_ME type  MARA-MEINS,
         I_MENGE type  EKPO-MENGE,
         E_MENG type  EKPO-MENGE,
        les_vbkok_lote TYPE vbkok,
        lti_vbpok_lote TYPE tab_vbpok,
        l_E_ERROR_IN_ITEM_DELETION TYPE xfeld,
        l_E_ERROR_IN_POD_UPDATE       TYPE xfeld,
        l_E_ERROR_IN_INTERFACE TYPE xfeld,
        l_E_ERROR_IN_GOODS_ISSUE TYPE xfeld,
        l_E_ERROR_IN_FINAL_CHECK  TYPE xfeld,
        l_E_ERROR_PARTNER_UPDATE TYPE xfeld,
        l_E_ERROR_SERNR_UPDATE   TYPE xfeld,
*}   INSERT
        l_e_msg TYPE string,
        les_return TYPE zesd_msg_picking,
        lti_prott TYPE tab_prott,
        les_prott TYPE prott,
        les_bapiret1 TYPE bapiret1,
        l_e_numsg TYPE n LENGTH 3,
        l_e_cant TYPE zedsd_cant_doc,
        lti_mara  TYPE TABLE OF mara,
        les_mara  TYPE mara.

  FIELD-SYMBOLS: <lfs_t_entregas> TYPE zesd_det_ent_pkg,
                 <lfs_picking_det> TYPE zedsd_picking_det,
                 <lfs_picking_det_ubic> TYPE zedsd_picking_det,
                 <lfs_detalle> TYPE zesd_eanmat.
*.....Tipo Posicion particion de Lotes
  CONSTANTS : lcte_tiposicion TYPE pstyv_vl VALUE 'YB99'.

*.....Consulto el Pikum de la Entrega
  CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
    EXPORTING
      i_entrega = i_entrega-entrega
    RECEIVING
      e_picknum = l_e_piknum.
*.....Creo Instancia de Clase
  IF  lo_lgtica_picking IS NOT BOUND  .
    CREATE OBJECT lo_lgtica_picking .
  ENDIF.
*.....Cargo Atributos Picking
  lo_lgtica_picking->load_picking(
    EXPORTING
      i_picknum = l_e_piknum ).

*.....Inicia Proceso de Actualización de Entrega
*.....Consulto Cabecera Documento Picking
  les_picking_cab = lo_lgtica_picking->s_picking.

  IF  les_picking_cab-estado NE 'CERRADO'.

*.....Información de Datos de Cabecera
    IF cabeceras_ventas[] IS NOT INITIAL .
      READ TABLE cabeceras_ventas INDEX 1 INTO les_cab_ventas.
    ELSEIF cabeceras_compras[] IS NOT INITIAL.
      READ TABLE cabeceras_compras INDEX 1 INTO les_cab_compras.
    ENDIF.

*.....Consulto Detalle Documento Picking para llenar tabla de Función
    lti_picking_det = lo_lgtica_picking->t_picking_det.
*.....Consulto Seriales
    lti_picking_ser = lo_lgtica_picking->t_picking_ser.
*    lti_picking_ser_aux[] = lti_picking_ser.
*.....Consulto Lotes
    lti_picking_lotes = lo_lgtica_picking->t_picking_lot.
    lti_picking_lotes_aux[] = lti_picking_lotes.
*.....Ordeno Tabla
    SORT lti_picking_det BY vbeln2 posicion ASCENDING.

*.....Completo datos para la Estructura vbkok_wa  de la Función (CABECERA)
    les_vbkok-vbeln_vl = les_picking_cab-vbeln2. "Número Entrega
    les_vbkok-vbtyp_vl = 'J'.                    "Tipo de Documento Comercial
    les_vbkok-vbeln = les_picking_cab-vbeln2.    "Orden de picking
    les_vbkok-lgnum =  i_entrega-lgnum.          "Núm.almacén/Complejo alm.
    les_vbkok-komue = 'X'.                       "Ajuste automático de cantidad entrega a cantidad picking
    les_vbkok-wabuc = 'X'.                       "Contabilizar movimiento de mercancías automáticamente

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
    LOOP AT zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas>.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lfs_t_entregas>-matnr
        IMPORTING
          output = les_mara-matnr.

      APPEND les_mara TO lti_mara.
*{   INSERT         ER3K900309                                        8
      IF <lfs_t_entregas>-charg is not INITIAL and <lfs_t_entregas>-pstyv NE lcte_tiposicion .
        LOOP AT lti_picking_lotes INTO les_picking_lotes WHERE matnr = <lfs_t_entregas>-matnr and TIPO_DOC ne 'TRAS' .

*        READ TABLE lti_picking_lotes INTO les_picking_lotes
*                                    WITH KEY matnr = <lfs_t_entregas>-matnr lote = <lfs_t_entregas>-charg.
        IF les_picking_lotes-lote NE <lfs_t_entregas>-charg.

*        CONCATENATE 'Revisar Datos para la Entrega:' space les_picking_cab-vbeln2 space 'Error: No se ha leído el lote esperado' space
*        <lfs_t_entregas>-charg space INTO l_e_msg RESPECTING BLANKS.
         MESSAGE S001(ZCLPCK) WITH les_picking_cab-vbeln2 <lfs_t_entregas>-charg into l_e_msg .
        les_return-num_doc = i_entrega-documento.
        les_return-msg = l_e_msg.
        les_return-type_msg = 'E'.
        APPEND les_return TO c_return.
        ENDIF.
        ENDLOOP.
      ENDIF.
*}   INSERT
    ENDLOOP.
*{   INSERT         ER3K900309                                        9

     IF c_return is not INITIAL.
       EXIT.
     ENDIF.
     IF I_TIPO_DOCUMENTO eq 'TRAS'.
        lti_picking_det_x = lti_picking_det.
        DELETE lti_picking_det_x WHERE CANTCONT = 0.
        IF lti_picking_det_x is INITIAL.
            les_flag_tras_del = 'X'.
        ENDIF.
     ENDIF.
*}   INSERT

    LOOP AT lti_picking_det ASSIGNING <lfs_picking_det>.
      les_mara-matnr = <lfs_picking_det>-material.
      APPEND les_mara TO lti_mara.
    ENDLOOP.

*.....Función para Obtener EANs de un Material
        CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
          TABLES
            t_mara    = lti_mara.
*.....FIN_MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*.....Elimino Posiciones Correspondientes a Particion de Lotes
*.....Inserto en Estructuras para Actualizar la Entrega
    LOOP AT zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas>
*{   REPLACE        ER3K900309                                        7
*\                                            WHERE ( pstyv EQ lcte_tiposicion OR charg IS NOT INITIAL ) AND entrega = les_picking_cab-vbeln2.
*                                            WHERE ( pstyv EQ lcte_tiposicion ) AND entrega = les_picking_cab-vbeln2.
                                              WHERE entrega = les_picking_cab-vbeln2.

      IF <lfs_t_entregas>-pstyv ne lcte_tiposicion.
        READ TABLE lti_picking_lotes INTO les_picking_lotes
                                    WITH KEY matnr = <lfs_t_entregas>-matnr.
        IF sy-subrc eq 0.
          IF les_picking_lotes-TIPO_DOC eq 'TRAS'.
            IF <lfs_t_entregas>-charg is initial.
            CONTINUE.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ELSE.
          IF <lfs_t_entregas>-pstyv eq 'NLN'.
            IF <lfs_t_entregas>-charg is initial.
            CONTINUE.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        ENDIF.


      ENDIF.

*}   REPLACE

      les_vbpok_eli-vbeln_vl = les_picking_cab-vbeln2.    "Entrega
      les_vbpok_eli-posnr_vl = <lfs_t_entregas>-posnr.    "Posición de entrega
      les_vbpok_eli-vbeln = les_picking_cab-vbeln2.       "Documento comercial subsiguiente

      les_vbpok_eli-posnn = <lfs_t_entregas>-vgpos.     "Posición siguiente del documento comercial

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lfs_t_entregas>-matnr
        IMPORTING
          output = les_vbpok_eli-matnr.   "Número de material




      les_vbpok_eli-taqui = 'X'.                          "Indicador: Confirmación de pedidos de transporte MM-MW
      les_vbpok_eli-lianp = 'X'.                          "Modificar cantidad de entrega
      les_vbpok_eli-lips_del = 'X'.                       "Flag: Borrar posición de entrega
      les_vbpok_eli-lfimg = <lfs_t_entregas>-lfimg.       "Cantidad entregada efectivamente en UMV
      les_vbpok_eli-wms_rfbel = <lfs_t_entregas>-entrega. "Número de documento del documento modelo
      les_vbpok_eli-wms_rfpos = <lfs_t_entregas>-posnr   ."Núm.posición de la posición modelo
      APPEND les_vbpok_eli TO lti_vbpok.
      CLEAR : les_vbpok_eli.
    ENDLOOP.
*.....Recorro Tabla de Posiciones Picking
    LOOP AT lti_picking_det ASSIGNING <lfs_picking_det>.
      l_e_cant_cont = l_e_cant_cont + <lfs_picking_det>-cantcont.
*.....Nivel de Ruptura para Posiciones en la Tabla de Picking
      AT END OF posicion.
        IF les_cab_ventas IS NOT INITIAL.

*.....Consulto si el Material Tiene Lotes para Ventas (Lleno Estructura con Datos de Lotes)
          LOOP AT lti_picking_lotes INTO les_picking_lotes
                                    WHERE matnr = <lfs_picking_det>-material AND
                                          vbeln = les_cab_ventas-vbeln AND cantidad_uso > 0.

            l_e_control = 1.
            les_vbpok_lote-vbeln_vl = les_picking_cab-vbeln2.    "Entrega
            les_vbpok_lote-posnr_vl = <lfs_picking_det>-posicion.  "Posición de entrega
            les_vbpok_lote-vbeln = les_picking_cab-vbeln2.       "Documento comercial subsiguiente
            les_vbpok_lote-posnn = <lfs_picking_det>-posicion.     "Posición siguiente del documento comercial

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = les_vbpok_lote-matnr.   "Número de material

*.....Número de Lote
            les_vbpok_lote-charg = les_picking_lotes-lote.       "Número de lote
            les_vbpok_lote-taqui = 'X'.                          "Indicador: Confirmación de pedidos de transporte MM-MW
            les_vbpok_lote-lianp = 'X'.                          "Modificar cantidad de entrega
*{   REPLACE        ER3K900337                                       37
*\            les_vbpok_lote-lips_del = ' '.                       "Flag: Borrar posición de entrega
            IF l_e_cant_cont EQ 0.
                les_vbpok_lote-lips_del = 'X'.                       "Flag: Borrar posición de entrega
            ELSE.
              les_vbpok_lote-lips_del = ' '.                       "Flag: Borrar posición de entrega
            ENDIF.
*}   REPLACE
*....       Si la cantidad de las tablas de picking son menores o iguales a la cantidad que hay en el lote
*....       Asigno la Cantidad de las tablas de picking y resto esa cantidad al lote
            IF l_e_cant_cont <= les_picking_lotes-cantidad_uso.
              les_vbpok_lote-lfimg = l_e_cant_cont .     "Cantidad entregada efectivamente en UMV
              les_picking_lotes-cantidad_uso = les_picking_lotes-cantidad_uso - l_e_cant_cont.
              l_e_cant_cont = 0.
            ELSE.
*....       Si la cantidad de las tablas de picking es mayor a la cantidad que hay en el lote
*....       Asigno la Cantidad del lote , pongo cantidad de lote en 0 y resto a la cantidad de las tablas de picking
*...        la cantidad del lote
              les_vbpok_lote-lfimg = les_picking_lotes-cantidad_uso.     "Cantidad entregada efectivamente en UMV
              l_e_cant_cont = l_e_cant_cont - les_picking_lotes-cantidad_uso.
              les_picking_lotes-cantidad_uso = 0.
            ENDIF.

**            les_vbpok_lote-lfimg = les_picking_lotes-cantidad.     "Cantidad entregada efectivamente en UMV
            les_vbpok_lote-wms_rfbel = les_picking_cab-vbeln2.   "Número de documento del documento modelo
            les_vbpok_lote-wms_rfpos = <lfs_picking_det>-posicion. "Núm.posición de la posición modelo
            les_picking_lotes-vbeln2 = les_picking_cab-vbeln2.
*.....Elimino de Tabla de Lotes el Registro Procesado

            MODIFY lti_picking_lotes FROM les_picking_lotes INDEX sy-tabix.
*{   INSERT         ER3K900309                                       27
*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.

*            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*             EXPORTING
*               INPUT                = <lfs_picking_det>-umc
*              LANGUAGE             = SY-LANGU
*            IMPORTING
*              OUTPUT               = LES_VBPOK_lote-MEINS
*                     .
*
             READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.
*             IF sy-subrc eq 0.
*               LES_VBPOK_lote-VRKME = <lfs_t_entregas>-VRKME.
*             ELSE.
*               LES_VBPOK_lote-VRKME = LES_VBPOK_lote-MEINS.
*             ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = <lfs_picking_det>-umc
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
            EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = <lfs_picking_det>-umc.
* Implement suitable error handling here
             ENDIF.

              I_MATNR = l_e_nmat.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = <lfs_t_entregas>-VRKME.
              I_MENGE = les_vbpok_lote-lfimg.

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

                 les_vbpok_lote-lfimg = E_MENG.
*           I_GOODSMVT_ITEM-ENTRY_UOM = WA_EKPO-MEINS.
*           I_GOODSMVT_ITEM-ENTRY_QNT = CEIL( I_GOODSMVT_ITEM-ENTRY_QNT ).
*                les_vbpok_lote-LGPLA = les_picking_lotes-ubicacion_fi.
*}   INSERT


*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.

*... Proceso para descontar de la Z Ubicaciones ( ZMM_T_UBICACIONES ) (se modifica el with key de la consulta Ajuste SO-128)
            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.
            IF sy-subrc EQ 0.

*            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = <lfs_picking_det>-material posnr = <lfs_picking_det>-posicion.
*            IF sy-subrc EQ 0.


*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
              CLEAR l_e_nmat.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <lfs_t_entregas>-matnr
                IMPORTING
                  output = l_e_nmat.


*.... Consulto el registro de la z de ubicaciones (se modifica el where de la consulta Ajuste SO-128)
*{   REPLACE        ER3K900309                                       12
*\              SELECT SINGLE *
*\                INTO les_det_ubicacion
*\                FROM zmm_t_ubicacion
*\                 WHERE material EQ l_e_nmat AND
*\                  centro EQ <lfs_t_entregas>-werks AND
*\                  almacen EQ <lfs_t_entregas>-lgort AND
*\*                 lote eq les_picking_lotes-lote  and
*\                  ubicacion EQ les_picking_lotes-ubicacion_fi.
              SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                            NUMERO_DOC USUARIO  FECHA HORA
                INTO les_det_ubicacion
                FROM zmm_t_ubicacion
                 WHERE material EQ l_e_nmat AND
                  centro EQ <lfs_t_entregas>-werks AND
                  almacen EQ <lfs_t_entregas>-lgort AND
*                 lote eq les_picking_lotes-lote  and
                  ubicacion EQ les_picking_lotes-ubicacion_fi.
*}   REPLACE

*              SELECT SINGLE *
*                INTO les_det_ubicacion
*                FROM zmm_t_ubicacion
*                 WHERE material EQ <lfs_t_entregas>-matnr AND
*                  centro EQ <lfs_t_entregas>-werks AND
*                  almacen EQ <lfs_t_entregas>-lgort AND
**                 lote eq les_picking_lotes-lote  and
*                  ubicacion EQ les_picking_lotes-ubicacion_fi.

*   FIN SO-128 LFY 07/04/2015

*.... Si hay registro descuento la cantidad consumida
              IF sy-subrc EQ 0.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = <lfs_t_entregas>-matnr
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.

                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lotes-umc.
                IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900337                                       33
*\                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_vbpok_lote-lfimg * <lfs_detalle>-cantidad_ean ).
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( I_MENGE * <lfs_detalle>-cantidad_ean ).
*}   REPLACE
                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ELSE.
*.... Válido valor inconvertible
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input    = les_picking_lotes-umc
                      language = sy-langu
                    IMPORTING
                      output   = les_picking_lotes-umc.
                  READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lotes-umc.
                  IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900337                                       34
*\                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_vbpok_lote-lfimg * <lfs_detalle>-cantidad_ean ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( I_MENGE * <lfs_detalle>-cantidad_ean ).
*}   REPLACE
                    MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                  ENDIF.
                ENDIF.

              ENDIF.

            ENDIF.
            APPEND les_vbpok_lote TO lti_vbpok.
            CLEAR : les_picking_lotes, les_vbpok_lote.

          ENDLOOP.
          CLEAR:l_e_cont_cant_ser.
*.....Consulto si el Material tiene Seriales para Ventas
          LOOP AT lti_picking_ser INTO les_picking_ser
                                 WHERE matnr = <lfs_picking_det>-material AND
                                       vbeln = les_cab_ventas-vbeln AND flag_ser NE 'X'.

            IF l_e_cant_cont = l_e_cont_cant_ser.
              CONTINUE.
            ENDIF.
            les_sernr_update-rfbel = les_picking_cab-vbeln2.    "Documento referencia
            les_sernr_update-rfpos = <lfs_picking_det>-posicion.  "Posición de referencia
            les_sernr_update-sernr = les_picking_ser-serie.     "Número de serie

            APPEND les_sernr_update TO lti_sernr_update.
            les_picking_ser-flag_ser = 'X'.
            les_picking_ser-vbeln2 = les_picking_cab-vbeln2.
            APPEND les_picking_ser TO lti_picking_ser_aux.
            l_e_cont_cant_ser = l_e_cont_cant_ser + 1.
*.....Elimino Material Serializado de tabla de Seriales
            DELETE lti_picking_ser WHERE matnr = <lfs_picking_det>-material AND
                                         serie = les_picking_ser-serie AND
                                         vbeln = les_cab_ventas-vbeln.
            CLEAR : les_sernr_update,  les_picking_ser.
          ENDLOOP.
        ELSEIF les_cab_compras IS NOT INITIAL.
*.....Consulto si el Material Tiene Lotes para Compras
          LOOP AT lti_picking_lotes INTO les_picking_lotes
                                    WHERE matnr = <lfs_picking_det>-material AND
                                          ebeln = les_cab_compras-ebeln AND cantidad_uso > 0.
*            l_e_cant_cont = l_e_cant_cont + <lfs_picking_det>-CANTCONT.
            l_e_control = 1.
            les_vbpok_lote-vbeln_vl = les_picking_cab-vbeln2.    "Entrega
            les_vbpok_lote-posnr_vl = <lfs_picking_det>-posicion.  "Posición de entrega
            les_vbpok_lote-vbeln = les_picking_cab-vbeln2.       "Documento comercial subsiguiente
            les_vbpok_lote-posnn = <lfs_picking_det>-posicion.     "Posición siguiente del documento comercial

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = les_vbpok_lote-matnr.   "Número de material

*.....Número de Lote
            les_vbpok_lote-charg = les_picking_lotes-lote.       "Número de lote
            les_vbpok_lote-taqui = 'X'.                          "Indicador: Confirmación de pedidos de transporte MM-MW
            les_vbpok_lote-lianp = 'X'.                          "Modificar cantidad de entrega
            les_vbpok_lote-lips_del = ' '.                       "Flag: Borrar posición de entrega
*....       Si la cantidad de las tablas de picking son menores o iguales a la cantidad que hay en el lote
*....       Asigno la Cantidad de las tablas de picking y resto esa cantidad al lote
            IF l_e_cant_cont <= les_picking_lotes-cantidad_uso.
              les_vbpok_lote-lfimg = l_e_cant_cont .     "Cantidad entregada efectivamente en UMV
              les_picking_lotes-cantidad_uso = les_picking_lotes-cantidad_uso - l_e_cant_cont.
              l_e_cant_cont = 0.
            ELSE.
*....       Si la cantidad de las tablas de picking es mayor a la cantidad que hay en el lote
*....       Asigno la Cantidad del lote , pongo cantidad de lote en 0 y resto a la cantidad de las tablas de picking
*...        la cantidad del lote
              les_vbpok_lote-lfimg = les_picking_lotes-cantidad_uso.     "Cantidad entregada efectivamente en UMV
              l_e_cant_cont = l_e_cant_cont - les_picking_lotes-cantidad_uso.
              les_picking_lotes-cantidad_uso = 0.
            ENDIF.

*            les_vbpok_lote-lfimg = les_picking_lotes-cantidad.     "Cantidad entregada efectivamente en UMV
            les_vbpok_lote-wms_rfbel = les_picking_cab-vbeln2.   "Número de documento del documento modelo
            les_vbpok_lote-wms_rfpos = <lfs_picking_det>-posicion. "Núm.posición de la posición modelo
            les_picking_lotes-vbeln2 = les_picking_cab-vbeln2.
*.....Elimino de Tabla de Lotes el Registro Procesado

            MODIFY lti_picking_lotes FROM les_picking_lotes INDEX sy-tabix.
*{   INSERT         ER3K900309                                       28
*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.
*            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*             EXPORTING
*               INPUT                = <lfs_picking_det>-umc
*              LANGUAGE             = SY-LANGU
*            IMPORTING
*              OUTPUT               = LES_VBPOK_lote-MEINS
*                     .
*
             READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.
*             IF sy-subrc eq 0.
*               LES_VBPOK_lote-VRKME = <lfs_t_entregas>-VRKME.
*             ELSE.
*               LES_VBPOK_lote-VRKME = LES_VBPOK_lote-MEINS.
*             ENDIF.
              CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = <lfs_picking_det>-umc
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
              EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = <lfs_picking_det>-umc.
* Implement suitable error handling here
             ENDIF.
              I_MATNR = l_e_nmat.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = <lfs_t_entregas>-VRKME.
              I_MENGE = les_vbpok_lote-lfimg.

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

                 les_vbpok_lote-lfimg = E_MENG.
*                 les_vbpok_lote-LGPLA = les_picking_lotes-ubicacion_fi.
*}   INSERT
*            IF les_picking_lotes-cantidad EQ 0.
*              DELETE lti_picking_lotes WHERE  matnr = <lfs_picking_det>-material AND
*                                            lote = les_picking_lotes-lote AND
*                                            vbeln = les_cab_ventas-vbeln.
*            ENDIF.

*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.


*... Proceso para descontar de la Z Ubicaciones ( ZMM_T_UBICACIONES )(se modifica el with key de la consulta Ajuste SO-128)
            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.


*            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = <lfs_picking_det>-material posnr = <lfs_picking_det>-posicion.

            IF sy-subrc EQ 0.

*............Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
              CLEAR l_e_nmat.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <lfs_t_entregas>-matnr
                IMPORTING
                  output = l_e_nmat.


*.... Consulto el registro de la z de ubicaciones (se modifica el where de la consulta Ajuste SO-128)
*{   REPLACE        ER3K900309                                       13
*\              SELECT SINGLE *
*\               INTO les_det_ubicacion
*\               FROM zmm_t_ubicacion
*\                WHERE material EQ l_e_nmat AND
*\                 centro EQ <lfs_t_entregas>-werks AND
*\                 almacen EQ <lfs_t_entregas>-lgort AND
*\*                 lote eq les_picking_lotes-lote  and
*\                 ubicacion EQ les_picking_lotes-ubicacion_fi.
              SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                            NUMERO_DOC USUARIO  FECHA HORA
               INTO les_det_ubicacion
               FROM zmm_t_ubicacion
                WHERE material EQ l_e_nmat AND
                 centro EQ <lfs_t_entregas>-werks AND
                 almacen EQ <lfs_t_entregas>-lgort AND
*                 lote eq les_picking_lotes-lote  and
                 ubicacion EQ les_picking_lotes-ubicacion_fi.
*}   REPLACE

*              SELECT SINGLE *
*                INTO les_det_ubicacion
*                FROM zmm_t_ubicacion
*                 WHERE material EQ <lfs_t_entregas>-matnr AND
*                  centro EQ <lfs_t_entregas>-werks AND
*                  almacen EQ <lfs_t_entregas>-lgort AND
**                 lote eq les_picking_lotes-lote  and
*                  ubicacion EQ les_picking_lotes-ubicacion_fi.

*               FIN SO-128 LFY 07/04/2015

*.... Si hay registro descuento la cantidad consumida
              IF sy-subrc EQ 0.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = <lfs_t_entregas>-matnr
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.


                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lotes-umc.
                IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900337                                       35
*\                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_vbpok_lote-lfimg * <lfs_detalle>-cantidad_ean ).
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( I_MENGE * <lfs_detalle>-cantidad_ean ).
*}   REPLACE
                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ELSE.
*.... Válido valor inconvertible
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input    = les_picking_lotes-umc
                      language = sy-langu
                    IMPORTING
                      output   = les_picking_lotes-umc.
                  READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lotes-umc.
                  IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900337                                       36
*\                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_vbpok_lote-lfimg * <lfs_detalle>-cantidad_ean ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( I_MENGE * <lfs_detalle>-cantidad_ean ).
*}   REPLACE
                    MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                  ENDIF.
                ENDIF.

              ENDIF.

            ENDIF.
            APPEND les_vbpok_lote TO lti_vbpok.
            CLEAR : les_picking_lotes, les_vbpok_lote.

          ENDLOOP.
          CLEAR:l_e_cont_cant_ser.
*.....Consulto si el Material tiene Seriales para Ventas
          LOOP AT lti_picking_ser INTO les_picking_ser
                                  WHERE matnr = <lfs_picking_det>-material AND
                                        ebeln = les_cab_compras-ebeln AND flag_ser NE 'X'.
            IF l_e_cant_cont = l_e_cont_cant_ser.
              CONTINUE.
            ENDIF.
            les_sernr_update-rfbel = les_picking_cab-vbeln2.      "Documento referencia
            les_sernr_update-rfpos = <lfs_picking_det>-posicion.  "Posición de referencia
            les_sernr_update-sernr = les_picking_ser-serie.       "Número de serie
            l_e_cont_cant_ser = l_e_cont_cant_ser + 1.
            APPEND les_sernr_update TO lti_sernr_update.
*            APPEND les_picking_ser TO lti_picking_ser_aux.
*.....Elimino Material Serializado de tabla de Seriales
            DELETE lti_picking_ser WHERE matnr = <lfs_picking_det>-material AND
                                         serie = les_picking_ser-serie AND
                                         ebeln = les_cab_compras-ebeln.
            les_picking_ser-flag_ser = 'X'.
            les_picking_ser-vbeln2 = les_picking_cab-vbeln2.
            APPEND les_picking_ser TO  lti_picking_ser_aux.

            CLEAR :  les_sernr_update, les_picking_ser.
          ENDLOOP.
        ENDIF.
*.....Datos para Posiciones que no Tienen Lote ni seriales
        IF l_e_control IS INITIAL .
          les_vbpok-vbeln_vl = les_picking_cab-vbeln2.    "Entrega
          les_vbpok-posnr_vl = <lfs_picking_det>-posicion.  "Posición de entrega
          les_vbpok-vbeln = les_picking_cab-vbeln2.       "Documento comercial subsiguiente
          les_vbpok-posnn = <lfs_picking_det>-posicion.     "Posición siguiente del documento comercial
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <lfs_picking_det>-material
            IMPORTING
              output = les_vbpok-matnr.   "Número de material
*          les_vbpok-matnr = <lfs_picking_det>-material.     "Número de material
          les_vbpok-taqui = 'X'.                          "Indicador: Confirmación de pedidos de transporte MM-MW
          les_vbpok-lianp = 'X'.                          "Modificar cantidad de entrega

          IF <lfs_picking_det>-cantcont IS INITIAL.
            les_vbpok-lips_del = 'X'.                     "Flag: Borrar posición de entrega
*{   INSERT         ER3K900309                                       30
            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY ENTREGA = les_vbpok-vbeln_vl POSNR = les_vbpok-posnr_vl
            MATNR = les_vbpok-matnr.
            IF sy-subrc eq 0.
*              les_vbpok-posnn = <lfs_t_entregas>-vgpos.     "Posición siguiente del documento comercial
            ENDIF.

*}   INSERT
          ELSE.
            les_vbpok-lips_del = ' '.                     "Flag: Borrar posición de entrega
          ENDIF.
*.....Verifico la Cantidad Contada
          IF l_e_cant_pos IS NOT INITIAL .
            les_vbpok-lfimg = l_e_cant_pos + <lfs_picking_det>-cantcont .
          ELSE.
            les_vbpok-lfimg = <lfs_picking_det>-cantcont.     "Cantidad entregada efectivamente en UMV
          ENDIF.
*{   INSERT         ER3K900309                                       29
*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.
*            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*             EXPORTING
*               INPUT                = <lfs_picking_det>-umc
*              LANGUAGE             = SY-LANGU
*            IMPORTING
*              OUTPUT               = LES_VBPOK-MEINS
*                     .
*
             READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.
*             IF sy-subrc eq 0.
*               LES_VBPOK-VRKME = <lfs_t_entregas>-VRKME.
*             ELSE.
*               LES_VBPOK-VRKME = LES_VBPOK-MEINS.
*             ENDIF.
              CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
             EXPORTING
               INPUT                = <lfs_picking_det>-umc
              LANGUAGE             = SY-LANGU
            IMPORTING
              OUTPUT               = I_IN_ME
              EXCEPTIONS
                UNIT_NOT_FOUND       = 1
                OTHERS               = 2.
             IF SY-SUBRC <> 0.
               I_IN_ME = <lfs_picking_det>-umc.
* Implement suitable error handling here
             ENDIF.

              I_MATNR = l_e_nmat.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = <lfs_t_entregas>-VRKME.
              I_MENGE = les_vbpok-lfimg.

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
*             les_vbpok-lfimg = FLOOR( E_MENG ).
*           ELSE.
*             les_vbpok-lfimg = E_MENG.
*           ENDIF.
*
                            IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.

                    lvs = ( E_MENG * 1000 ) MOD 10 .
                    IF  lvs >= 1  AND lvs < 5.
                        E_MENG = E_MENG -  ( lvs / 1000 ).
                    ELSEIF lvs > 4  AND lvs <= 9 .
                        E_MENG = E_MENG + ( ( 10 - lvs ) / 1000 ).

                    ENDIF.

                 ENDIF.

                 les_vbpok-lfimg = E_MENG.
*                 les_vbpok-LGPLA = les_picking_lotes-ubicacion_fi.
*}   INSERT

          les_vbpok-wms_rfbel = les_picking_cab-vbeln2.   "Número de documento del documento modelo
          les_vbpok-wms_rfpos = <lfs_picking_det>-posicion. "Núm.posición de la posición modelo
          IF les_vbpok-lips_del NE 'X'.


*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CLEAR l_e_nmat.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = l_e_nmat.


*... Proceso para descontar de la Z Ubicaciones ( ZMM_T_UBICACIONES )(se modifica el with key de la consulta Ajuste SO-128)
            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = l_e_nmat posnr = <lfs_picking_det>-posicion.

*            READ TABLE zif_lgtica_documento~t_entregas ASSIGNING <lfs_t_entregas> WITH KEY entrega =  les_picking_cab-vbeln2 matnr = <lfs_picking_det>-material posnr = <lfs_picking_det>-posicion.

            IF sy-subrc EQ 0.
              LOOP AT lti_picking_det ASSIGNING <lfs_picking_det_ubic> WHERE posicion = <lfs_picking_det>-posicion.


*............Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
                CLEAR l_e_nmat.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <lfs_t_entregas>-matnr
                  IMPORTING
                    output = l_e_nmat.

*.... Consulto el registro de la z de ubicaciones (se modifica el where de la consulta Ajuste SO-128)
*{   REPLACE        ER3K900309                                       14
*\                SELECT SINGLE *
*\                  INTO les_det_ubicacion
*\                  FROM zmm_t_ubicacion
*\                   WHERE material EQ l_e_nmat AND
*\                    centro EQ <lfs_t_entregas>-werks AND
*\                    almacen EQ <lfs_t_entregas>-lgort AND
*\*                 lote eq les_picking_lotes-lote  and
*\                    ubicacion EQ <lfs_picking_det_ubic>-ubicacion_fi.
                SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                              NUMERO_DOC USUARIO  FECHA HORA
                  INTO les_det_ubicacion
                  FROM zmm_t_ubicacion
                   WHERE material EQ l_e_nmat AND
                    centro EQ <lfs_t_entregas>-werks AND
                    almacen EQ <lfs_t_entregas>-lgort AND
*                 lote eq les_picking_lotes-lote  and
                    ubicacion EQ <lfs_picking_det_ubic>-ubicacion_fi.
*}   REPLACE


*                SELECT SINGLE *
*                  INTO les_det_ubicacion
*                  FROM zmm_t_ubicacion
*                   WHERE material EQ <lfs_t_entregas>-matnr AND
*                    centro EQ <lfs_t_entregas>-werks AND
*                    almacen EQ <lfs_t_entregas>-lgort AND
**                 lote eq les_picking_lotes-lote  and
*                    ubicacion EQ <lfs_picking_det_ubic>-ubicacion_fi.


*                  FIN SO-128 LFY 07/04/2015

*.... Si hay registro descuento la cantidad consumida
                IF sy-subrc EQ 0.
*.....Limpieza de Variables
                  CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                    EXPORTING
                      matnr      = <lfs_t_entregas>-matnr
                    TABLES
                      t_detalle  = lti_detalle
                      t_mensajes = lti_mensajes.

                  READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_picking_det_ubic>-umc.
                  IF sy-subrc EQ 0 .
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_picking_det_ubic>-cantcont * <lfs_detalle>-cantidad_ean ).
                    MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                  ELSE.
*.... Válido valor inconvertible
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input    = <lfs_picking_det_ubic>-umc
                        language = sy-langu
                      IMPORTING
                        output   = <lfs_picking_det_ubic>-umc.
                    READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_picking_det_ubic>-umc.
                    IF sy-subrc EQ 0 .
                      les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_picking_det_ubic>-cantcont * <lfs_detalle>-cantidad_ean ).
                      MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ENDLOOP.
            ENDIF.

          ENDIF.

          APPEND les_vbpok TO lti_vbpok.
          CLEAR: les_vbpok .
        ENDIF.
        CLEAR : l_e_control, l_e_cant_pos ,l_e_cant_cont.
        l_e_cont_cant = 1.
      ENDAT.

      IF l_e_cont_cant IS INITIAL .
        l_e_cant_pos = l_e_cant_pos + <lfs_picking_det>-cantcont.
      ELSE.
        CLEAR : l_e_cont_cant.
      ENDIF.
    ENDLOOP.
*{   INSERT         ER3K900309                                        3
   IF lti_vbpok_lote is not INITIAL.
     les_vbkok_lote = les_vbkok.
     CALL FUNCTION 'SERIAL_INTTAB_REFRESH'
     EXPORTING
     OBJECTS_STATUS_REFRESH       = 'E'.

*.....Llamado a Función que Actualiza Entrega y la contabiliza
*.....Según la Informacion de Tablas de Pickig.
    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa                     = les_vbkok
        delivery                     = i_entrega-entrega
        update_picking               = ' '
        synchron                     = 'X'
        commit                       = ' '
        if_get_delivery_buffered     = 'X'
        if_no_generic_system_service = 'X'
        if_database_update_1         = '1'
        if_error_messages_send       = ' '
        if_no_buffer_refresh         = 'X'
        it_sernr_update              = lti_sernr_update
      IMPORTING
        ef_error_any                 = l_e_error_any
        EF_ERROR_IN_ITEM_DELETION    = l_E_ERROR_IN_ITEM_DELETION
        EF_ERROR_IN_POD_UPDATE       = l_E_ERROR_IN_POD_UPDATE
        EF_ERROR_IN_INTERFACE        = l_E_ERROR_IN_INTERFACE
        EF_ERROR_IN_GOODS_ISSUE      = l_E_ERROR_IN_GOODS_ISSUE
        EF_ERROR_IN_FINAL_CHECK      = l_E_ERROR_IN_FINAL_CHECK
        EF_ERROR_PARTNER_UPDATE      = l_E_ERROR_PARTNER_UPDATE
        EF_ERROR_SERNR_UPDATE        = l_E_ERROR_SERNR_UPDATE

      TABLES
        vbpok_tab                    = lti_vbpok      "Tabla con Datos de Entrega
        prot                         = lti_prott      "Mensajes de Error
      EXCEPTIONS
        error_message                = 99.

     IF sy-subrc EQ 0 AND l_e_error_any NE 'X'.
       CALL FUNCTION 'SERIAL_INTTAB_REFRESH'
       EXPORTING
       OBJECTS_STATUS_REFRESH       = 'E'.
*.....Llamado a Función que Actualiza Entrega y la contabiliza
*.....Según la Informacion de Tablas de Pickig.
    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa                     = les_vbkok_lote
        delivery                     = i_entrega-entrega
        update_picking               = 'X'
        synchron                     = 'X'
        commit                       = ' '
        if_get_delivery_buffered     = 'X'
        if_no_generic_system_service = 'X'
        if_database_update_1         = '1'
        if_error_messages_send       = ' '
        if_no_buffer_refresh         = 'X'
        it_sernr_update              = lti_sernr_update
      IMPORTING
        ef_error_any                 = l_e_error_any
        EF_ERROR_IN_ITEM_DELETION    = l_E_ERROR_IN_ITEM_DELETION
        EF_ERROR_IN_POD_UPDATE       = l_E_ERROR_IN_POD_UPDATE
        EF_ERROR_IN_INTERFACE        = l_E_ERROR_IN_INTERFACE
        EF_ERROR_IN_GOODS_ISSUE      = l_E_ERROR_IN_GOODS_ISSUE
        EF_ERROR_IN_FINAL_CHECK      = l_E_ERROR_IN_FINAL_CHECK
        EF_ERROR_PARTNER_UPDATE      = l_E_ERROR_PARTNER_UPDATE
        EF_ERROR_SERNR_UPDATE        = l_E_ERROR_SERNR_UPDATE

      TABLES
        vbpok_tab                    = lti_vbpok_lote      "Tabla con Datos de Entrega
        prot                         = lti_prott      "Mensajes de Error
      EXCEPTIONS
        error_message                = 99.
          IF sy-subrc EQ 0 AND
    l_e_error_any NE 'X'.
*.....Elimino Registros de Tablas de Seriales y Lotes, cuando el Proceso es Exitoso
      IF lti_picking_lotes_aux[] IS NOT INITIAL .
*        DELETE lti_picking_lotes where cantidad > 0.
        MODIFY zmm_t_pck_lot FROM TABLE lti_picking_lotes.
      ENDIF.
      IF lti_picking_ser_aux[] IS  NOT INITIAL.
        MODIFY zmm_t_pck_ser FROM TABLE lti_picking_ser_aux.
      ENDIF.
*... Proceso para descontar de la Z Ubicaciones ( ZMM_T_UBICACIONES )
*    IF cabeceras_ventas[] IS NOT INITIAL .
**      LOOP AT .
**
**      ENDLOOP.
*    ELSEIF cabeceras_compras[] IS NOT INITIAL.
*
*    ENDIF.
      MESSAGE S002(ZCLPCK) WITH i_entrega-entrega i_entrega-DOCUMENTO into l_e_msg .
*      CONCATENATE 'Se Actualizo la Entrega' space i_entrega-entrega space 'del Documento' space
*      i_entrega-documento space INTO l_e_msg RESPECTING BLANKS.
      les_return-num_doc = i_entrega-documento.
      les_return-msg = l_e_msg.
      les_return-type_msg = 'S'.
      APPEND les_return TO c_return.
*.....Cerrar Picking Para Procesos que Generan Entrega, excepto Proyectos,
*.....Estos Cierran Picking cuando se consume el Inventario(MIGO) Metodo Contabilizar Ordenes
      IF zif_lgtica_documento~tipo_documento NE 'PROY' .
        lo_lgtica_picking->cerrar_picking(
          EXPORTING
            i_picknum = l_e_piknum
            i_usuario = lo_lgtica_picking->s_picking-usuario ).
      ENDIF.
    ELSE.
      IF lti_prott IS NOT INITIAL .
        LOOP AT lti_prott INTO les_prott .
          l_e_numsg = les_prott-msgno.
          CALL FUNCTION 'BALW_BAPIRETURN_GET1'
            EXPORTING
              type       = les_prott-msgty
              cl         = les_prott-msgid
              number     = l_e_numsg
            IMPORTING
              bapireturn = les_bapiret1.

          CONCATENATE 'Entrega Número:' space i_entrega-entrega space les_bapiret1-message space
          INTO l_e_msg RESPECTING BLANKS.
          les_return-num_doc = i_entrega-documento.
          les_return-msg = l_e_msg.
*..... si no se han transferido posiciones advertimos
          IF l_e_numsg EQ '610'.
            les_return-type_msg = 'I'.
          ELSE.
            les_return-type_msg = les_bapiret1-type.
          ENDIF.

          APPEND les_return TO c_return.
        ENDLOOP.
      ELSE.
*        CONCATENATE 'Revisar Datos para la Entrega:' space i_entrega-entrega space 'Error: La Entrega' space
*        'No se ha Contabilizado' space INTO l_e_msg RESPECTING BLANKS.
        MESSAGE E003(ZCLPCK) WITH i_entrega-entrega into l_e_msg .
        les_return-num_doc = i_entrega-documento.
        les_return-msg = l_e_msg.
        les_return-type_msg = 'E'.
        APPEND les_return TO c_return.
      ENDIF.
*.....Deshacer Cambios realiazados en Bases de Datos
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ROLLBACK WORK.
    ENDIF.
     ELSE.
        IF lti_prott IS NOT INITIAL .
        LOOP AT lti_prott INTO les_prott .
          l_e_numsg = les_prott-msgno.
          CALL FUNCTION 'BALW_BAPIRETURN_GET1'
            EXPORTING
              type       = les_prott-msgty
              cl         = les_prott-msgid
              number     = l_e_numsg
            IMPORTING
              bapireturn = les_bapiret1.

          CONCATENATE 'Entrega Número:' space i_entrega-entrega space les_bapiret1-message space
          INTO l_e_msg RESPECTING BLANKS.
          les_return-num_doc = i_entrega-documento.
          les_return-msg = l_e_msg.
*..... si no se han transferido posiciones advertimos
          IF l_e_numsg EQ '610'.
            les_return-type_msg = 'I'.
          ELSE.
            les_return-type_msg = les_bapiret1-type.
          ENDIF.

          APPEND les_return TO c_return.
        ENDLOOP.
      ELSE.
*        CONCATENATE 'Revisar Datos para la Entrega:' space i_entrega-entrega space 'Error: La Entrega' space
*        'No se ha Contabilizado' space INTO l_e_msg RESPECTING BLANKS.
        MESSAGE E003(ZCLPCK) WITH i_entrega-entrega into l_e_msg .
        les_return-num_doc = i_entrega-documento.
        les_return-msg = l_e_msg.
        les_return-type_msg = 'E'.
        APPEND les_return TO c_return.
      ENDIF.
*.....Deshacer Cambios realiazados en Bases de Datos
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ROLLBACK WORK.
     ENDIF.

   ELSE.
     CALL FUNCTION 'SERIAL_INTTAB_REFRESH'
     EXPORTING
     OBJECTS_STATUS_REFRESH       = 'E'.
     IF les_flag_tras_del EQ 'X'.
       les_vbkok-likp_del = 'X'.
     ENDIF.
     LOOP AT lti_vbpok into les_vbpok.
*       clear: les_vbpok-charg.
       COLLECT les_vbpok INTO lti_vbpok_lote.
     ENDLOOP.
     lti_vbpok = lti_vbpok_lote.
*
*     LOOP AT zif_lgtica_documento~t_entregas INTO les_entrega.
*
*     ENDLOOP.
*}   INSERT

*.....Llamado a Función que Actualiza Entrega y la contabiliza
*.....Según la Informacion de Tablas de Pickig.
    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa                     = les_vbkok
        delivery                     = i_entrega-entrega
        update_picking               = 'X'
*{   REPLACE        ER3K900309                                        5
*\        synchron                     = ' '
*\        commit                       = ' '
        synchron                     = 'X'
        commit                       = ' '
*}   REPLACE
        if_get_delivery_buffered     = 'X'
        if_no_generic_system_service = 'X'
        if_database_update_1         = '1'
        if_error_messages_send       = ' '
        if_no_buffer_refresh         = 'X'
        it_sernr_update              = lti_sernr_update
      IMPORTING
*{   REPLACE        ER3K900309                                        6
*\        ef_error_any                 = l_e_error_any
        ef_error_any                 = l_e_error_any
        EF_ERROR_IN_ITEM_DELETION    = l_E_ERROR_IN_ITEM_DELETION
        EF_ERROR_IN_POD_UPDATE       = l_E_ERROR_IN_POD_UPDATE
        EF_ERROR_IN_INTERFACE        = l_E_ERROR_IN_INTERFACE
        EF_ERROR_IN_GOODS_ISSUE      = l_E_ERROR_IN_GOODS_ISSUE
        EF_ERROR_IN_FINAL_CHECK      = l_E_ERROR_IN_FINAL_CHECK
        EF_ERROR_PARTNER_UPDATE      = l_E_ERROR_PARTNER_UPDATE
        EF_ERROR_SERNR_UPDATE        = l_E_ERROR_SERNR_UPDATE
*}   REPLACE
      TABLES
        vbpok_tab                    = lti_vbpok      "Tabla con Datos de Entrega
        prot                         = lti_prott      "Mensajes de Error
      EXCEPTIONS
        error_message                = 99.

*{   REPLACE        ER3K900309                                       32
*\    IF sy-subrc EQ 0 AND
*\    l_e_error_any NE 'X'.
*    IF l_e_error_any NE 'X' and l_E_ERROR_IN_INTERFACE eq 'X' and les_flag_tras_del eq 'X'.
*
*    ENDIF.
    IF sy-subrc EQ 0 AND
    l_e_error_any NE 'X' OR ( l_e_error_any EQ 'X' and l_E_ERROR_IN_GOODS_ISSUE eq 'X' and les_flag_tras_del eq 'X').
*}   REPLACE
*.....Elimino Registros de Tablas de Seriales y Lotes, cuando el Proceso es Exitoso
      IF lti_picking_lotes_aux[] IS NOT INITIAL .
*        DELETE lti_picking_lotes where cantidad > 0.
        MODIFY zmm_t_pck_lot FROM TABLE lti_picking_lotes.
      ENDIF.
      IF lti_picking_ser_aux[] IS  NOT INITIAL.
        MODIFY zmm_t_pck_ser FROM TABLE lti_picking_ser_aux.
      ENDIF.
*... Proceso para descontar de la Z Ubicaciones ( ZMM_T_UBICACIONES )
*    IF cabeceras_ventas[] IS NOT INITIAL .
**      LOOP AT .
**
**      ENDLOOP.
*    ELSEIF cabeceras_compras[] IS NOT INITIAL.
*
*    ENDIF.
*{   REPLACE        ER3K900309                                       17
*\      CONCATENATE 'Se Actualizo la Entrega' space i_entrega-entrega space 'del Documento' space
*\      i_entrega-documento space INTO l_e_msg RESPECTING BLANKS.

      MESSAGE S002(ZCLPCK) WITH i_entrega-entrega i_entrega-documento into l_e_msg .
*}   REPLACE
      les_return-num_doc = i_entrega-documento.
      les_return-msg = l_e_msg.
      les_return-type_msg = 'S'.
      APPEND les_return TO c_return.
*.....Cerrar Picking Para Procesos que Generan Entrega, excepto Proyectos,
*.....Estos Cierran Picking cuando se consume el Inventario(MIGO) Metodo Contabilizar Ordenes
      IF zif_lgtica_documento~tipo_documento NE 'PROY' .
        lo_lgtica_picking->cerrar_picking(
          EXPORTING
            i_picknum = l_e_piknum
            i_usuario = lo_lgtica_picking->s_picking-usuario ).
      ENDIF.
    ELSE.
      IF lti_prott IS NOT INITIAL .
        LOOP AT lti_prott INTO les_prott .
          l_e_numsg = les_prott-msgno.
          CALL FUNCTION 'BALW_BAPIRETURN_GET1'
            EXPORTING
              type       = les_prott-msgty
              cl         = les_prott-msgid
              number     = l_e_numsg
            IMPORTING
              bapireturn = les_bapiret1.

          CONCATENATE 'Entrega Número:' space i_entrega-entrega space les_bapiret1-message space
          INTO l_e_msg RESPECTING BLANKS.
          les_return-num_doc = i_entrega-documento.
          les_return-msg = l_e_msg.
*..... si no se han transferido posiciones advertimos
          IF l_e_numsg EQ '610'.
            les_return-type_msg = 'I'.
          ELSE.
            les_return-type_msg = les_bapiret1-type.
          ENDIF.

          APPEND les_return TO c_return.
        ENDLOOP.
      ELSE.
*{   REPLACE        ER3K900309                                       15
*\        CONCATENATE 'Revisar Datos para la Entrega:' space i_entrega-entrega space 'Error: La Entrega' space
*\        'No se ha Contabilizado' space INTO l_e_msg RESPECTING BLANKS.
        MESSAGE E003(ZCLPCK) WITH i_entrega-entrega into l_e_msg .
*}   REPLACE
        les_return-num_doc = i_entrega-documento.
        les_return-msg = l_e_msg.
        les_return-type_msg = 'E'.
        APPEND les_return TO c_return.
      ENDIF.
*.....Deshacer Cambios realiazados en Bases de Datos
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*{   INSERT         ER3K900309                                        4
       ROLLBACK WORK.
         ENDIF.
*}   INSERT
    ENDIF.
  ELSEIF  les_picking_cab IS INITIAL.
*{   REPLACE        ER3K900309                                       16
*\    CONCATENATE 'La Entrega:' space i_entrega-entrega space 'No se Encuentra Registrada en Tabla de Picking' space INTO l_e_msg RESPECTING BLANKS.
    MESSAGE E003(ZCLPCK) WITH i_entrega-entrega  into l_e_msg .
*}   REPLACE
    les_return-num_doc = i_entrega-documento.
    les_return-msg = l_e_msg.
    les_return-type_msg = 'E'.
    APPEND les_return TO c_return.
  ENDIF.
*.....Limpieza de Variables
  CLEAR: les_vbkok, lti_sernr_update, lti_vbpok, l_e_error_any, les_prott, les_bapiret1, les_return,
         l_e_msg, lti_prott, l_e_numsg, les_cab_ventas, les_cab_compras, lti_picking_lotes_aux,
         lti_picking_ser_aux, l_e_cant.

ENDMETHOD.


method ADD_CANTIDADES1.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para registrar las cantidades contadas con el dispositivo
*
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*********************************************************************************
* Definición de Constantes
*-------------------------------------------------------------------------------*
*CONSTANT:

*-------------------------------------------------------------------------------*
* Definición de Tipos Locales
*-------------------------------------------------------------------------------*
*TYPES: ….



*-------------------------------------------------------------------------------*
* Definición de Estructuras Tables
*-------------------------------------------------------------------------------*


*-------------------------------------------------------------------------------*
* Definición de Estructuras
*-------------------------------------------------------------------------------*
*
DATA: les_est_conteo type zmm_t_est_conteo.
DATA: les_cab_picking type ZMM_T_PICKING.
DATA: les_det_picking type ZMM_T_PICKING_DT.
DATA: les_c_reg_posiciones type ZMM_T_PICKING_DT.
*-------------------------------------------------------------------------------*
* Definición de Tablas Internas
*-------------------------------------------------------------------------------*
DATA: lti_est_conteo type STANDARD TABLE OF zmm_t_est_conteo.
DATA: lti_cab_picking type STANDARD TABLE OF ZMM_T_PICKING.
DATA: lti_det_picking type STANDARD TABLE OF ZMM_T_PICKING_DT.
*-------------------------------------------------------------------------------*
* Definición de Variables
*-------------------------------------------------------------------------------*


*... Validar que se haya contado almenos un material
IF c_reg_posiciones[] is not initial.

***  Valido el estado del documento en la tabla de estados
       refresh lti_est_conteo.
*{   REPLACE        ER3K900279                                        3
*\        select *
        select MANDT USUARIO  NUMERO_DOC PROCESO ESTADO FECHA HORA P_CONFIRM  SUBPROCESO
*}   REPLACE
          into table lti_est_conteo
          from zmm_t_est_conteo
          where usuario    eq i_usuario
            and numero_doc eq i_documento_origen
            and estado eq 'TERMINADO'.

else.
*... Si  no se envían conteos , se eliminan
   READ TABLE c_reg_posiciones into les_det_picking index 1.
*  leo la entrega obtengo el picknum asociado a la entrega
*         call method me->GET_PICKNUM
*          exporting
*            entrega = i_documento_origen
*            usuario = i_usuario
*          exporting
*            PICKNUM = i_documento_origen.

ENDIF.

*.....Ajusto Número de Documento
        call method me->get_ajustar_documento
          exporting
            i_documento = i_documento_origen
          receiving
            r_documento = i_documento_origen.

*... Selecciono los registros que esten guardados en la cabecera de del documento de picking
*{   REPLACE        ER3K900279                                        1
*\select *
select MANDT PICKNUM PROCESO  SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
*}   REPLACE
  into table lti_cab_picking
  from ZMM_T_PICKING
  where vbeln2 eq i_entrega.

  IF sy-subrc eq 0.
*... Selecciono los detalles que esten guardados en la cabecera de del documento de picking
*{   REPLACE        ER3K900279                                        2
*\    select *
    select MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO
           CANTCONT UMCC  P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO
*}   REPLACE
  into table lti_det_picking
  from ZMM_T_PICKING_dt
  for all entries in lti_cab_picking
  where PICKNUM eq lti_cab_picking-picknum.


  ENDIF.
*... Recorro las posiciones enviadas y cotejo con las guardadas
LOOP AT c_reg_posiciones into les_c_reg_posiciones.
       CLEAR:lti_det_picking.
       Read table lti_det_picking with key PICKNUM = les_c_reg_posiciones-PICKNUM POSPICK = les_c_reg_posiciones-POSPICK into les_det_picking.
       IF sy-subrc eq 0.
* Aumento conteo
            add 1 to les_det_picking-conteo.
       else.
            les_det_picking-conteo = 1.
       ENDIF.
ENDLOOP.


endmethod.


METHOD call_bapi_delivery1.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Genera Entregas para Tx VL10_
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 07.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 07.07.2014    ER6K906905    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
*.....Tablas Internas con la Informacion de Retorno
     lti_sd_order TYPE shp_vl10_sd_order,
     lti_vbsk_all TYPE shp_vbsk_t,
     lti_key_late TYPE shp_vl10_package_t.

*.....Función para Generar Entrega
  CALL FUNCTION 'SHP_VL10_DELIVERY_CREATE'
    EXPORTING
      if_ledat           = i_ledat
      if_nur_vorgabe_pos = i_nur_vorgabe_pos
      it_key_enque_read  = i_key_enque_read
    TABLES
      it_vetvg           = i_vetvg
    CHANGING
      cx_sd_order        = lti_sd_order
      ct_vbsk_all        = e_vbsk
      ct_vbls            = e_vbls
      ct_key_late        = lti_key_late.
*{   INSERT         ER3K900309                                        1
    E_SYST = SY.
*}   INSERT

ENDMETHOD.


METHOD call_bapi_delivery2.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Geneerar Entregas para Tx ME2O
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 04.07.2014    ER6K906899    Marco Suarez        Creación
*-------------------------------------------------------------------------------*


*.....Informacíon de Respuesta de la Función
  DATA: lti_xvbfs TYPE TABLE OF vbfs,
        lti_xvbls TYPE TABLE OF vbls.

*.....Llamado a Función Encargada de Generar Entrega
  CALL FUNCTION 'GN_DELIVERY_CREATE'
    EXPORTING
      vbsk_i               = i_vbsk_i
      no_commit            = ' '
      vbls_pos_rueck       = 'X'
      if_no_deque          = 'X'
    TABLES
      xkomdlgn             = i_xkomdlgn "Interfase de entrega general: Tabla de comunicación
      xvbfs                = lti_xvbfs  "Protocolo de errores proceso colectivo
      xvbls                = lti_xvbls  "Notas de entrega para proceso colectivo
      xxlips               = r_xlips.   "Doc.comercial: Entrega - Datos de posición
*{   INSERT         ER3K900309                                        1
      LTI_VBFS = lti_xvbfs.
      E_SYST  = sy.
*}   INSERT

ENDMETHOD.


METHOD constructor.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Constructor
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 26.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 26.06.2014    ER6K906849    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Recibo Typo de Proceso
  zif_lgtica_documento~tipo_documento = i_tipo_doc.

*.....Llamado a Metodo para Obtener los Detalles de Usuario
  CALL METHOD me->get_detalles_usuario
    EXPORTING
      i_usuario     = i_usuario
    IMPORTING
      e_det_usuario = detalles_usuario
    CHANGING
      c_mensajes    = mensajes.

ENDMETHOD.


METHOD contabiliza_aglutinador.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado Contabilizar Documentos Correspondientes a
*                Ordenes de Producción con Aglutinador
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 08.09.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 08.09.2014    ER6K907461    Marco A. Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
        les_return TYPE zesd_msg_picking,
        lti_cab_picknum TYPE zttsd_cab_pickum_aglu,
        l_e_msg TYPE string,
*.....Número material
        l_e_nmat    TYPE matnr, " SO-128 LFY 07/04/2015
*.....Cabecera Documento Picking
        les_cab_picking TYPE zedsd_picking,
*.....Detalle Posiciones Documento Picking
        lti_det_picking TYPE zttsd_picking_det,
        les_det_picking LIKE LINE OF lti_det_picking,
*.....Seriales Picking
        lti_ser_picking TYPE zttsd_picking_ser,
*.....Lotes Picking
        lti_lotes_picking TYPE zttsd_picking_lot,
*.....Variable de Ref a la Clase Picknum
        lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Cantidad Contada para cada Posicion
        l_e_cant_pos TYPE zedsd_cant_cnt,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.... Estructura de ubicaciones
        les_det_ubicacion TYPE zmm_t_ubicacion,
*{   INSERT         ER3K900279                                        9
        I_MATNR type  MARA-MATNR,
         I_IN_ME type  MARA-MEINS,
         I_OUT_ME type  MARA-MEINS,
         I_MENGE type  EKPO-MENGE,
         E_MENG type  EKPO-MENGE,
*}   INSERT

*.....Posicion
        l_e_pos TYPE n LENGTH 5,
*.....Control
        l_e_control TYPE i,
*.....Codigo Tx
        l_e_tx TYPE gm_code,
*.....Posicion Linea Documento Material
         l_e_linea TYPE mblpo,
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
        les_items TYPE bapi2017_gm_item_create,                "Estructura para Tabla de Pos. Reserv
        lti_mara  TYPE TABLE OF mara,
        les_mara  TYPE mara.

  FIELD-SYMBOLS : <lfs_cab_picknum> TYPE zedsd_cab_pickum_aglu,
                  <lfs_cab_aglu> TYPE zesd_cabecera_aglut,
                  <lfs_detalle_reservas> TYPE bapi2093_res_items_get,
<lfs_detalle> TYPE zesd_eanmat,
*.....Estructura para Lotes Picking
                  <lfs_lotes_picking> TYPE zedsd_picking_lot,
*.....Estructura para seriales picking
                  <lfs_ser_picking> TYPE zedsd_picking_ser,
*.....Estrctura para Detalles Picking
                  <lfs_det_picking> TYPE zedsd_picking_det,
*.....Estructura Mensajes de Respuesta BAPI
                  <lfs_return> TYPE bapiret2.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.

  LOOP AT detalles_reservas ASSIGNING <lfs_detalle_reservas>.
*.... Convertir material
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <lfs_detalle_reservas>-material
      IMPORTING
        output = les_mara-matnr.

    APPEND les_mara TO lti_mara.
  ENDLOOP.

*.....Función para Obtener EANs de un Material
  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
    TABLES
      t_mara = lti_mara.

*.....FIN_MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

*.....Lectura de Información de Aglutinador
  IF cabecera_aglutinador[] IS NOT INITIAL.
*.....Ordeno Tabla
    SORT  cabecera_aglutinador BY aglutinador ASCENDING.

*.....Elimino Registros Duplicados, solo en Necesario el Aglutinador
    DELETE ADJACENT DUPLICATES FROM cabecera_aglutinador COMPARING aglutinador.

*.....Consulto Lotes para el Aglutinador
*{   REPLACE        ER3K900279                                        3
*\    SELECT *
*\      FROM zmm_t_pck_lot
*\          INTO TABLE lti_lotes_picking
*\          FOR ALL ENTRIES IN cabecera_aglutinador
*\            WHERE aglutinador EQ cabecera_aglutinador-aglutinador AND
*\                  tipo_doc EQ zif_lgtica_documento~tipo_documento.
    SELECT MANDT DOCUMENTO TIPO_DOC MATNR  LOTE UBICACION_FI CANTIDAD UMC VBELN EBELN AGLUTINADOR
           VBELN2 AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
      FROM zmm_t_pck_lot
          INTO TABLE lti_lotes_picking
          FOR ALL ENTRIES IN cabecera_aglutinador
            WHERE aglutinador EQ cabecera_aglutinador-aglutinador AND
                  tipo_doc EQ zif_lgtica_documento~tipo_documento.
*}   REPLACE

*.....Consulto Seriales para el Aglutinador
*{   REPLACE        ER3K900279                                        4
*\    SELECT *
*\       FROM zmm_t_pck_ser
*\          INTO TABLE lti_ser_picking
*\            FOR ALL ENTRIES IN cabecera_aglutinador
*\                    WHERE aglutinador EQ  cabecera_aglutinador-aglutinador AND
*\                          tipo_doc EQ zif_lgtica_documento~tipo_documento.
    SELECT MANDT DOCUMENTO TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER
           UBICACION_FI
       FROM zmm_t_pck_ser
          INTO TABLE lti_ser_picking
            FOR ALL ENTRIES IN cabecera_aglutinador
                    WHERE aglutinador EQ  cabecera_aglutinador-aglutinador AND
                          tipo_doc EQ zif_lgtica_documento~tipo_documento.
*}   REPLACE

*.....Asigno Codigo de Transacción
    l_e_tx = '03'.

*.....Datos de Cabecera para la Contabilización de Ordenes y Reservas
    les_header-pstng_date = sy-datum.
    les_header-doc_date = sy-datum.
    les_header-pr_uname = detalles_usuario-usuario.
    les_code-gm_code = l_e_tx.

    LOOP AT cabecera_aglutinador ASSIGNING <lfs_cab_aglu>.
*.....Obtengo Información de Cabeceras para un Aglutinador
      CALL METHOD zcl_lgtica_picking=>get_data_by_aglutinador
        EXPORTING
          i_aglutinador         = <lfs_cab_aglu>-aglutinador
        RECEIVING
          r_picknum_aglutinador = lti_cab_picknum.

*.....Verifico que Exista Información en Tabla de Cabeceras de Aglutinadores
      IF lti_cab_picknum[] IS NOT INITIAL.
*.....Creo Instancia de Clase Picking
        IF  lo_lgtica_picking IS NOT BOUND  .
          CREATE OBJECT lo_lgtica_picking .
        ENDIF.

*.....Proceso Cada Picknum que Corresponde a una Orden de Producción con Aglutinador
        LOOP AT lti_cab_picknum ASSIGNING <lfs_cab_picknum>.
*.....Cargo Atributos Picking
          lo_lgtica_picking->load_picking(
            EXPORTING
              i_picknum = <lfs_cab_picknum>-picknum ).

*.....Datos Cabecera de Picking
          les_cab_picking = lo_lgtica_picking->s_picking.

          IF les_cab_picking-estado NE 'CERRADO'.
*.....Datos Detalle de Picking
            lti_det_picking = lo_lgtica_picking->t_picking_det.
*.....Ordeno Tabla
            SORT lti_det_picking BY posicion ASCENDING.
*{   INSERT         ER3K900279                                        8
            DELETE lti_det_picking where CANTCONT = 0.
*}   INSERT

*.....Recorro Tabla de Reservas por Número de Reserva y estado Pendiente por Picking
            LOOP AT detalles_reservas ASSIGNING <lfs_detalle_reservas>
                                       WHERE reserv_no EQ <lfs_cab_picknum>-rsnum AND
                                             movement EQ 'X'.
*.....Función para Ajuste de Número de Posicion
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <lfs_detalle_reservas>-res_item
                IMPORTING
                  output = l_e_pos.

              UNASSIGN <lfs_det_picking>.
*.... Valido que la posición sea OK , cantidad contada = cantidad a contar
              READ TABLE lti_det_picking ASSIGNING <lfs_det_picking> WITH KEY material = <lfs_detalle_reservas>-material
                                              posicion = l_e_pos p_confirm = 'OK'.
              IF <lfs_det_picking>  IS NOT ASSIGNED.
*.... Valido que la posición sea NM , Picking Parcial
                READ TABLE lti_det_picking ASSIGNING <lfs_det_picking> WITH KEY material = <lfs_detalle_reservas>-material
                                              posicion = l_e_pos p_confirm = 'NE'.
                IF <lfs_det_picking>  IS NOT ASSIGNED.
                  CONTINUE.
                ENDIF.

              ENDIF.

*.....POSICIÓN DOCUMENTO MATERIAL PARA TABLA SERIALES
              l_e_linea = l_e_linea + 1 .
*.....Tablas de Lotes
              LOOP AT lti_lotes_picking ASSIGNING <lfs_lotes_picking>
                                        WHERE matnr EQ <lfs_detalle_reservas>-material AND
                                              aglutinador EQ <lfs_cab_aglu>-aglutinador AND
                                              tipo_doc EQ zif_lgtica_documento~tipo_documento AND
*{   REPLACE        ER3K900279                                        1
*\                                              cantidad NE 0.
                                              CANTIDAD_USO > 0.
                <lfs_detalle_reservas>-entry_qnt = <lfs_det_picking>-CANTCONT.
*}   REPLACE
                l_e_control = 1.
*.....Material del Lote
                les_items-material = <lfs_detalle_reservas>-material.
*.....Centro Lote
                les_items-plant = <lfs_detalle_reservas>-plant.
*.....Almacen Lote
                les_items-stge_loc = <lfs_detalle_reservas>-stge_loc.
*.....Información de Lote
                les_items-batch = <lfs_lotes_picking>-lote.
*.....Cantidad Contada para el Lote
                les_items-entry_qnt = <lfs_detalle_reservas>-entry_qnt.
******Adiciono Unidad de medida
                les_items-entry_uom = <lfs_detalle_reservas>-entry_uom.
*.....Resto de la Tabla de Lotes la Cantidad Pendiente para la Reserva
*{   REPLACE        ER3K900279                                        2
*\                <lfs_lotes_picking>-cantidad = <lfs_lotes_picking>-cantidad - <lfs_detalle_reservas>-entry_qnt.

            IF <lfs_detalle_reservas>-entry_qnt <= <lfs_lotes_picking>-cantidad_uso.
              les_items-entry_qnt = <lfs_detalle_reservas>-entry_qnt .     "Cantidad entregada efectivamente en UMV
              <lfs_lotes_picking>-cantidad_uso = <lfs_lotes_picking>-cantidad_uso - <lfs_detalle_reservas>-entry_qnt.
              <lfs_detalle_reservas>-entry_qnt = 0.
            ELSE.
*....       Si la cantidad de las tablas de picking es mayor a la cantidad que hay en el lote
*....       Asigno la Cantidad del lote , pongo cantidad de lote en 0 y resto a la cantidad de las tablas de picking
*...        la cantidad del lote
              les_items-entry_qnt = <lfs_lotes_picking>-cantidad_uso.     "Cantidad entregada efectivamente en UMV
              <lfs_detalle_reservas>-entry_qnt = <lfs_detalle_reservas>-entry_qnt - <lfs_lotes_picking>-cantidad_uso.
              <lfs_lotes_picking>-cantidad_uso = 0.
            ENDIF.

            IF <lfs_detalle_reservas>-ENTRY_UOM NE <lfs_detalle_reservas>-ENTRY_UOM_ISO.
                CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
                EXPORTING
                  INPUT                = les_items-entry_uom
                 LANGUAGE             = SY-LANGU
               IMPORTING
                 OUTPUT               = I_IN_ME
               EXCEPTIONS
                   UNIT_NOT_FOUND       = 1
                   OTHERS               = 2.
                IF SY-SUBRC <> 0.
                  I_IN_ME = les_items-ENTRY_UOM.
*    Implement suitable error handling here
                ENDIF.

              I_MATNR = les_items-material.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = <lfs_detalle_reservas>-ENTRY_UOM_ISO.
              I_MENGE = les_items-entry_qnt.

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
*     Implement suitable error handling here
                 ENDIF.
               IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
                 les_items-entry_qnt = FLOOR( E_MENG ).
               ELSE.
                 les_items-entry_qnt = E_MENG.
               ENDIF.
               les_items-ENTRY_UOM  = <lfs_detalle_reservas>-ENTRY_UOM_ISO.
            ENDIF.

*                <lfs_lotes_picking>-CANTIDAD_USO = <lfs_lotes_picking>-CANTIDAD_USO - <lfs_detalle_reservas>-entry_qnt.
*}   REPLACE
*.....Número de Orden
                les_items-orderid = <lfs_cab_picknum>-aufnr.
*.....Posicion del Material en la Orden
                les_items-order_itno = <lfs_detalle_reservas>-res_item.
*.....Número de Reserva
                les_items-reserv_no = <lfs_detalle_reservas>-reserv_no.
*.....Posición del Material en la reserva
                les_items-res_item = <lfs_detalle_reservas>-res_item.
*.....Salida final de la reserva
*                les_items-withdrawn = 'X'.
*.....Tipo de Movimiento
                les_items-move_type = '261'.
*.....Elimino de Tabla de Lotes el Registro Procesado cuando no Tenga Cantidad
*                DELETE lti_lotes_picking WHERE  matnr EQ <lfs_detalle_reservas>-material AND
*                                                lote EQ <lfs_lotes_picking>-lote AND
*                                                aglutinador EQ <lfs_cab_aglu>-aglutinador AND
*                                                tipo_doc EQ zif_lgtica_documento~tipo_documento AND
*                                                cantidad = 0.


*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = les_items-material
                  IMPORTING
                    output = l_e_nmat.

*.... Consulto el registro de la z de ubicaciones
*{   REPLACE        ER3K900279                                        5
*\                SELECT SINGLE *
*\                  INTO les_det_ubicacion
*\                  FROM zmm_t_ubicacion
*\                   WHERE material EQ l_e_nmat AND
*\                    centro EQ les_items-plant AND
*\                    almacen EQ les_items-stge_loc AND
*\*                 lote eq les_picking_lotes-lote  and
*\                    ubicacion EQ <lfs_lotes_picking>-ubicacion_fi.
                SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                              NUMERO_DOC USUARIO  FECHA HORA
                  INTO les_det_ubicacion
                  FROM zmm_t_ubicacion
                   WHERE material EQ l_e_nmat AND
                    centro EQ les_items-plant AND
                    almacen EQ les_items-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                    ubicacion EQ <lfs_lotes_picking>-ubicacion_fi.
*}   REPLACE

*            SELECT SINGLE *
*              INTO les_det_ubicacion
*              FROM zmm_t_ubicacion
*               WHERE material EQ les_items-material AND
*                centro EQ les_items-plant AND
*                almacen EQ les_items-stge_loc AND
**                 lote eq les_picking_lotes-lote  and
*                ubicacion EQ <lfs_lotes_picking>-ubicacion_fi.

*        FIN SO-128 LFY 07/04/2015

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

                  READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_lotes_picking>-umc.
                  IF sy-subrc EQ 0 .
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                    MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                  ELSE.
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input    = <lfs_lotes_picking>-umc
                        language = sy-langu
                      IMPORTING
                        output   = <lfs_lotes_picking>-umc.
                    READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_lotes_picking>-umc.
                    IF sy-subrc EQ 0 .
                      les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                      MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                    ENDIF.
                  ENDIF.

                ENDIF.


                APPEND les_items TO lti_items.
*.....Limpieza de Variables
                CLEAR: les_items.
              ENDLOOP.

              IF l_e_control IS INITIAL .
*.....Función para Ajuste de Número de Posicion
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <lfs_detalle_reservas>-res_item
                  IMPORTING
                    output = l_e_pos.
                LOOP AT lti_det_picking ASSIGNING <lfs_det_picking>
                                         WHERE  material EQ <lfs_detalle_reservas>-material AND
                                                posicion EQ l_e_pos AND ( p_confirm EQ 'OK' OR p_confirm EQ 'NE') .

*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = <lfs_det_picking>-material
                    IMPORTING
                      output = l_e_nmat.

*.... Consulto el registro de la z de ubicaciones
*{   REPLACE        ER3K900279                                        6
*\                  SELECT SINGLE *
*\                    INTO les_det_ubicacion
*\                    FROM zmm_t_ubicacion
*\                     WHERE material EQ l_e_nmat AND
*\                      centro EQ <lfs_detalle_reservas>-plant AND
*\                      almacen EQ <lfs_detalle_reservas>-stge_loc AND
*\*                 lote eq les_picking_lotes-lote  and
*\                      ubicacion EQ <lfs_det_picking>-ubicacion_fi.
                  SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                                NUMERO_DOC USUARIO  FECHA HORA
                    INTO les_det_ubicacion
                    FROM zmm_t_ubicacion
                     WHERE material EQ l_e_nmat AND
                      centro EQ <lfs_detalle_reservas>-plant AND
                      almacen EQ <lfs_detalle_reservas>-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                      ubicacion EQ <lfs_det_picking>-ubicacion_fi.
*}   REPLACE


*                  SELECT SINGLE *
*                    INTO les_det_ubicacion
*                    FROM zmm_t_ubicacion
*                     WHERE material EQ <lfs_det_picking>-material AND
*                      centro EQ <lfs_detalle_reservas>-plant AND
*                      almacen EQ <lfs_detalle_reservas>-stge_loc AND
**                 lote eq les_picking_lotes-lote  and
*                      ubicacion EQ <lfs_det_picking>-ubicacion_fi.

*       FIN SO-128 LFY 07/04/2015

*.... Si hay registro descuento la cantidad consumida
                  IF sy-subrc EQ 0.
*.....Limpieza de Variables
                    CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                    CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                      EXPORTING
                        matnr      = <lfs_det_picking>-material
                      TABLES
                        t_detalle  = lti_detalle
                        t_mensajes = lti_mensajes.

                    READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_det_picking>-umc.
                    IF sy-subrc EQ 0 .
                      les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_det_picking>-cantcont * <lfs_detalle>-cantidad_ean ).
                      MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                    ENDIF.

                  ENDIF.

                  l_e_cant_pos =  l_e_cant_pos + <lfs_det_picking>-cantcont.
                ENDLOOP.

*.....Verifico que Encuentre Registro y que Tenga como minimo 1 Cantidad Contada
                IF l_e_cant_pos IS NOT INITIAL.
*.....Cantidad
                  les_items-entry_qnt = l_e_cant_pos . "les_det_picking-cantcont.
******Adiciono Unidad de medida
                  les_items-entry_uom = <lfs_detalle_reservas>-entry_uom.
*.....Material
                  les_items-material = <lfs_detalle_reservas>-material.
*.....Centro
                  les_items-plant = <lfs_detalle_reservas>-plant.
*.....Almacen
                  les_items-stge_loc = <lfs_detalle_reservas>-stge_loc.
*.....Lote
                  les_items-batch = <lfs_detalle_reservas>-batch.
*.....Número de Orden
                  les_items-orderid = <lfs_cab_picknum>-aufnr.
*.....Posicion del Material en la Orden
                  les_items-order_itno = <lfs_detalle_reservas>-res_item.
*.....Número de Reserva
                  les_items-reserv_no = <lfs_detalle_reservas>-reserv_no.
*.....Posición del Material en la reserva
                  les_items-res_item = <lfs_detalle_reservas>-res_item.
*.....Salida final de la reserva
*                  les_items-withdrawn = 'X'.
*.....Tipo de Movimiento
                  les_items-move_type = '261'.
*{   INSERT         ER3K900279                                       10
                IF <lfs_detalle_reservas>-ENTRY_UOM NE <lfs_detalle_reservas>-ENTRY_UOM_ISO.
                CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
                EXPORTING
                  INPUT                = les_items-entry_uom
                 LANGUAGE             = SY-LANGU
               IMPORTING
                 OUTPUT               = I_IN_ME
               EXCEPTIONS
                   UNIT_NOT_FOUND       = 1
                   OTHERS               = 2.
                IF SY-SUBRC <> 0.
                  I_IN_ME = les_items-ENTRY_UOM.
*    Implement suitable error handling here
                ENDIF.

              I_MATNR = les_items-material.
*              I_IN_ME = <lfs_picking_det>-umc.
              I_OUT_ME = <lfs_detalle_reservas>-ENTRY_UOM_ISO.
              I_MENGE = les_items-entry_qnt.

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
*     Implement suitable error handling here
                 ENDIF.
               IF I_IN_ME NE I_OUT_ME and I_IN_ME EQ 'KG' and I_OUT_ME EQ 'LB'.
                 les_items-entry_qnt = CEIL( E_MENG ).
               ELSE.
                 les_items-entry_qnt = E_MENG.
               ENDIF.
               les_items-ENTRY_UOM  = <lfs_detalle_reservas>-ENTRY_UOM_ISO.
            ENDIF.

*}   INSERT

                  APPEND les_items TO lti_items.
*.....Busqueda de Material Serializado
                  LOOP AT lti_ser_picking ASSIGNING <lfs_ser_picking>
                                           WHERE matnr = <lfs_detalle_reservas>-material AND
                                                 aglutinador = <lfs_cab_aglu>-aglutinador AND
                                                 tipo_doc EQ zif_lgtica_documento~tipo_documento AND
                                                 serie IS NOT INITIAL AND
                                                 flag_ser IS INITIAL.

                    les_serialnumber-matdoc_itm =  l_e_linea .
                    les_serialnumber-serialno = <lfs_ser_picking>-serie.
                    APPEND les_serialnumber TO lti_serialnumber.
*.....Flag de Confirmación Serie Usada para el Materialal
                    <lfs_ser_picking>-flag_ser  = 'X'.
*.....Elimino Material Serializado de tabla de Seriales
*                    DELETE lti_ser_picking WHERE matnr = <lfs_detalle_reservas>-material AND
*                                                 serie = <lfs_ser_picking>-serie AND
*                                                 tipo_doc EQ zif_lgtica_documento~tipo_documento.

                    CLEAR: les_serialnumber.
                  ENDLOOP.
                ENDIF.
              ENDIF.
              CLEAR : les_items,  l_e_pos, l_e_control, l_e_cant_pos.
            ENDLOOP.
            IF lti_items[] IS NOT INITIAL.


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

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = <lfs_cab_aglu>-aglutinador
                IMPORTING
                  output = <lfs_cab_aglu>-aglutinador.

*.....Existen Errores a Contabilizar la Orden
              IF lti_return[] IS NOT INITIAL.
                LOOP AT  lti_return ASSIGNING <lfs_return>.
                  les_return-num_doc = <lfs_cab_aglu>-aglutinador.
                  les_return-msg = <lfs_return>-message.
                  les_return-type_msg = <lfs_return>-type.
                  APPEND les_return TO c_return.
                  CLEAR : les_return.
                ENDLOOP.
*.....Deshacer Cambios realiazados en Bases de Datos
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*{   INSERT         ER3K900279                                       11
                ROLLBACK WORK.
*}   INSERT
              ELSE.
*.....Actualizo Tablas de Lotes
                UPDATE zmm_t_pck_lot FROM TABLE lti_lotes_picking.

*.....Actualizo Tabla de Seriales
                UPDATE zmm_t_pck_ser FROM TABLE lti_ser_picking.

                CONCATENATE 'Proceso Exitoso, Número de Documento Contabilización' space l_e_matnr space
                 INTO l_e_msg RESPECTING BLANKS.
                les_return-num_doc = <lfs_cab_aglu>-aglutinador.
                les_return-msg = l_e_msg.
                les_return-type_msg = 'S'.
                APPEND les_return TO c_return.
                CLEAR : les_return.
*.....Cerrar Picking
                lo_lgtica_picking->cerrar_picking(
                  EXPORTING
                    i_picknum = <lfs_cab_picknum>-picknum
                    i_usuario = lo_lgtica_picking->s_picking-usuario ).
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
              ENDIF.
*.....Limpieza de Variables
              CLEAR :les_headret, l_e_matnr,  l_e_year, lti_items, lti_serialnumber,
                     lti_return, les_cab_picking, lti_det_picking.
*{   INSERT         ER3K900279                                       12
            ELSE.
*.....Cerrar Picking
                lo_lgtica_picking->cerrar_picking(
                  EXPORTING
                    i_picknum = <lfs_cab_picknum>-picknum
                    i_usuario = lo_lgtica_picking->s_picking-usuario ).
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*}   INSERT
            ENDIF.
          ELSE.
            CONCATENATE 'Para el Aglutinador' space <lfs_cab_aglu>-aglutinador space
            'La Orden ' space les_cab_picking-aufnr
            space 'Esta Contabilizada y/o No esta Registrado en la Tabla de Picking' space INTO l_e_msg RESPECTING BLANKS.
            les_return-msg = l_e_msg.
*{   REPLACE        ER3K900279                                       13
*\            les_return-type_msg = 'E'.
            les_return-type_msg = 'I'.
*}   REPLACE
            APPEND les_return TO c_return.
            CLEAR : les_return.
          ENDIF.
        ENDLOOP.
*.....Elimino Registros de Tablas de Base de Datos (Lotes)
*        DELETE FROM zmm_t_pck_lot WHERE cantidad = 0.
*.....Elimino Registros de Tablas de Base de Datos (Seriales)
*        DELETE FROM zmm_t_pck_ser WHERE flag_ser EQ 'X'.
      ELSE.
        CONCATENATE 'No Existen Ordenes Pendientes por Picking para el Aglutinador '
        space <lfs_cab_aglu>-aglutinador space INTO l_e_msg RESPECTING BLANKS.
        les_return-num_doc = <lfs_cab_aglu>-aglutinador.
        les_return-msg = l_e_msg.
        les_return-type_msg = 'E'.
        APPEND les_return TO c_return.
      ENDIF.
    ENDLOOP.
  ELSE.
*    les_return-num_doc =
    les_return-msg = 'No Existe Información de Aglutinador'.
    les_return-type_msg = 'E'.
    APPEND les_return TO c_return.
  ENDIF.
ENDMETHOD.


METHOD CONTABILIZA_ENTREGA.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado de Generar OT Invisible y Contabilizar
*                Entrega (Salida de Mercancia)
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 23.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 23.07.2014    ER6K907037    Marco A. Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
*.....Tabla de Mensajes de Contabilizacion
        lti_bapiret1 TYPE bapiret1_tab,
*.....Mensaje
        l_e_msg TYPE string,
*.....Estructura Mensaje Respuesta
        les_return TYPE zesd_msg_picking.

  FIELD-SYMBOLS:
          <lfs_bapiret1> TYPE bapiret1.

*.....Metodo que Realiza la Contabilización de la Salida de la Mercancia
  CALL METHOD me->post_goods_issue
    EXPORTING
      i_entrega  = i_entrega-entrega
    CHANGING
      c_bapiret1 = lti_bapiret1.

  IF lti_bapiret1 IS NOT INITIAL .
    LOOP AT lti_bapiret1 ASSIGNING <lfs_bapiret1> .
      CONCATENATE 'Entrega Número:' space i_entrega-entrega space <lfs_bapiret1>-message space
       INTO l_e_msg RESPECTING BLANKS.
      les_return-num_doc = i_entrega-documento.
      les_return-msg = l_e_msg.
      les_return-type_msg = <lfs_bapiret1>-type.
      APPEND les_return TO c_return.
      CLEAR: les_return, l_e_msg.
    ENDLOOP.
  ENDIF.
*.....Limpieza de Variables
  CLEAR: lti_bapiret1.

ENDMETHOD.


METHOD contabiliza_orden.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado Contabilizar Documentos Correspondientes a
*                Ordenes de Servicio Internas, Proyectos
*                Ordenes de Prod sin Aglut, Reservas de Inventario
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 13.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 13.08.2014    ER6K907163    Marco A. Suarez        Creación
*-------------------------------------------------------------------------------*
  TYPES: BEGIN OF ty_resb,
    rsnum TYPE resb-rsnum,
    rspos TYPE resb-rspos,
    rsart TYPE resb-rsart,
    matnr TYPE resb-matnr,
    werks TYPE resb-werks,
    lgort TYPE resb-lgort,
    charg TYPE resb-charg,
    bdmng TYPE resb-bdmng,
    meins TYPE resb-meins,
    aufnr TYPE resb-aufnr,
    bwart TYPE resb-bwart,
    wempf TYPE resb-wempf,
    shkzg TYPE resb-shkzg,
  END OF ty_resb.

  DATA:
*.... Index para lotes
      l_e_index TYPE sy-tabix,
*.... Documento de origen para proyectos
     l_e_documento_origen TYPE ze_documento,
*.... Estructuras para buscar las reservas en la tabla resb
       lti_resb TYPE STANDARD TABLE OF ty_resb,
        les_resb TYPE ty_resb,
*..... Cantidad contada por posición de picking
        l_e_cant_cont TYPE zedsd_cant_cnt        ,
       les_msg_picking TYPE zesd_msg_picking,
*.....Mensaje
       l_e_msg TYPE string,
*.....Número Picknum
       l_e_material TYPE matnr,
*.....Número material
       l_e_nmat    TYPE matnr, " SO-128 LFY 07/04/2015
*.....Número Picknum
       l_e_piknum TYPE zed_picknum,
*.....Variable de Ref a la Clase Picknum
       lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Cabecera Documento Picking
       les_cab_picking TYPE zedsd_picking,
*.....Detalle Posiciones Documento Picking
       lti_det_picking TYPE zttsd_picking_det,
*.....Estructura para Detalle de Picking
       les_det_picking TYPE zedsd_picking_det,
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
*.....Cabecera Aglutinador
       les_cab_aglutinador TYPE zesd_cabecera_aglut,
*.....Cabecera Ventas
*{   REPLACE        ER3K900309                                        3
*\       les_cab_ventas TYPE vbak,
       les_cab_ventas TYPE ZESD_CAB_PED,
*}   REPLACE
*.....Posicion
       l_e_pos TYPE n LENGTH 5,
*.....Posicion mat reserva
       l_e_pos_res TYPE n LENGTH 4,
*.....Control
       l_e_control TYPE i,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
           lti_mara     TYPE TABLE OF mara,
           les_mara     TYPE mara,
*.... Estructura de ubicaciones
        les_det_ubicacion TYPE zmm_t_ubicacion,
*.....Cantidad Contada para cada Posicion
        l_e_cant_pos TYPE zedsd_cant_cnt,
*.....Control para la Cantidad de seriales
        l_e_cont_cant_ser TYPE i,
*.....Posicion Linea Documento Material
        l_e_linea TYPE mblpo,
*.....Codigo Tx
       l_e_tx TYPE gm_code,
*.....Detalles Entrega
       les_det_entrega TYPE zesd_det_ent_pkg,
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

  FIELD-SYMBOLS:
                 <lfs_picking_det> TYPE zedsd_picking_det,
         <lfs_detalle_reservas> TYPE bapi2093_res_items_get,
         <lfs_detalle> TYPE zesd_eanmat,
         <lfs_return> TYPE bapiret2.

*.....Proceso Correspondiente a Ordenes de Producción con Aglutinador
  IF zif_lgtica_documento~tipo_documento EQ 'OPAG'.
    me->contabiliza_aglutinador(
      CHANGING
        c_return = c_return ).
  ELSE.
*.....Ordenes de Servicio Internas, Sin Aglutinador.
    IF cabeceras_ordenes IS NOT INITIAL.
*.....Consulto el Picknum de una Orden
      CALL METHOD zcl_lgtica_picking=>get_data_by_orden
        EXPORTING
          i_orden   = cabeceras_ordenes-aufnr
        RECEIVING
          e_picknum = l_e_piknum.

*.....Reservervas de Inventario
    ELSEIF cabecera_reservas IS NOT INITIAL.
      CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
        EXPORTING
          i_rsnum   = cabecera_reservas-rsnum
        RECEIVING
          e_picknum = l_e_piknum.
*....  Reservas
      SELECT  rsnum rspos rsart matnr werks lgort charg bdmng meins aufnr bwart wempf shkzg
        INTO TABLE lti_resb
        FROM resb
        WHERE rsnum EQ cabecera_reservas-rsnum
          AND xloek EQ ''
          AND kzear EQ ''.
*.....Consulto el Piknum de una Entrega (Ordenes de Servicio en Proyectos)
    ELSEIF i_entrega IS NOT INITIAL.
      CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
        EXPORTING
          i_entrega = i_entrega-entrega
        RECEIVING
          e_picknum = l_e_piknum.
*.....Cabecera de Ventas
      READ TABLE cabeceras_ventas INDEX 1 INTO les_cab_ventas.

*.... Convertir documento
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = les_cab_ventas-vbeln
        IMPORTING
          output = l_e_documento_origen.

*.....Metodo para Setiar Atributos de entregas
      CALL METHOD me->zif_lgtica_documento~set_entregas
        EXPORTING
          i_clase_documento  = ''
          i_documento_origen = l_e_documento_origen.
    ENDIF.

*.....Creo Instancia de Clase Picking
    IF  lo_lgtica_picking IS NOT BOUND  .
      CREATE OBJECT lo_lgtica_picking .
    ENDIF.

*.....Cargo Atributos Picking
    lo_lgtica_picking->load_picking(
      EXPORTING
        i_picknum = l_e_piknum ).

*.....Datos Cabecera de Picking
    les_cab_picking = lo_lgtica_picking->s_picking.

    IF  les_cab_picking-estado NE 'CERRADO' AND
        les_cab_picking IS NOT INITIAL.

*.....Datos Detalle de Picking
      lti_det_picking = lo_lgtica_picking->t_picking_det.
*.....Ordeno Tabla
      SORT lti_det_picking BY posicion ASCENDING.
*.....Datos Seriales de Picking
      lti_ser_picking = lo_lgtica_picking->t_picking_ser.
      lti_picking_ser_aux[] = lti_ser_picking.
*......Datos de Lote de Picking
      lti_lotes_picking = lo_lgtica_picking->t_picking_lot.
      lti_picking_lotes_aux[] = lti_lotes_picking.

*.....Identifico el Tipo de Doc para Asigarle el Cod. de Tx, y valores a la estructura
      IF zif_lgtica_documento~tipo_documento EQ 'OPSG' OR
         zif_lgtica_documento~tipo_documento EQ 'RINV' OR
         zif_lgtica_documento~tipo_documento EQ 'PROY'.
        l_e_tx = '03'.
      ELSEIF     zif_lgtica_documento~tipo_documento EQ 'OSIN'.
        l_e_tx = '06'.
      ENDIF.
*.....Si el Atributo Detalles Reservas es Inicial, entoces es una Orden de Servicio en Proyectos
      IF detalles_reservas IS NOT INITIAL .
*.....Datos de Cabecera para la Contabilización de Ordenes y Reservas
        les_header-pstng_date = sy-datum.
        les_header-doc_date = sy-datum.
        les_header-pr_uname = detalles_usuario-usuario.
        les_code-gm_code = l_e_tx.
*{   INSERT         ER3K900309                                        9
         DELETE detalles_reservas WHERE plant NE detalles_usuario-centro.
         DELETE detalles_reservas WHERE stge_loc ne detalles_usuario-almacen.
*}   INSERT

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.

        LOOP AT detalles_reservas ASSIGNING <lfs_detalle_reservas>.
*.... Convertir material
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <lfs_detalle_reservas>-material
            IMPORTING
              output = les_mara-matnr.

          APPEND les_mara TO lti_mara.
        ENDLOOP.

        LOOP AT lti_det_picking INTO les_det_picking.
          les_mara-matnr = les_det_picking-material.
          APPEND les_mara TO lti_mara.
        ENDLOOP.

*.....Función para Obtener EANs de un Material
        CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
          TABLES
            t_mara    = lti_mara.

*.....FIN_MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

***********************        Inicio Cambio de Recorrido
*.....Recorro Tabla de Posiciones Picking
    LOOP AT lti_det_picking ASSIGNING <lfs_picking_det> where p_confirm NE 'CT'.
*..... Acumulo la cantidad total de la posición del documento
      l_e_cant_cont = l_e_cant_cont + <lfs_picking_det>-cantcont.

      AT END OF posicion.
*{   INSERT         ER3K900309                                        6
        IF l_e_cant_cont eq 0.
          CONTINUE.
        ENDIF.
*}   INSERT
*.....Función para Ajuste de Número de Posicion
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-posicion
              IMPORTING
                output = l_e_pos_res.
*..... Leer el registro si es un tipo de documento RSIN
          READ TABLE lti_resb INTO les_resb WITH KEY rsnum  = <lfs_picking_det>-RSNUM rspos = l_e_pos_res.
          IF sy-subrc EQ 0.
            IF ( les_resb-bwart EQ '591' OR les_resb-bwart EQ '592' OR les_resb-bwart EQ '301' ) AND zif_lgtica_documento~tipo_documento EQ 'RINV'.
              les_code-gm_code = '06'.
            ENDIF.
          ENDIF.

          READ TABLE detalles_reservas ASSIGNING <lfs_detalle_reservas> WITH KEY reserv_no = <lfs_picking_det>-RSNUM res_item = l_e_pos_res.
*.... Convertir material
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <lfs_detalle_reservas>-material
            IMPORTING
              output = l_e_material.
*.... Línea para asignar Seriales
          DESCRIBE TABLE lti_items LINES l_e_linea.
          l_e_linea = l_e_linea + 1.
*.....Recorro Tabla de Lotes en caso que Exista Posiciones con Lotes
          LOOP AT lti_lotes_picking INTO les_lotes_picking
                                    WHERE matnr = l_e_material AND
                                         ( aufnr = cabeceras_ordenes-aufnr OR
                                            rsnum = cabecera_reservas-rsnum ) AND cantidad_uso > 0.

            l_e_index = sy-tabix.
            l_e_control = 1.
*.....Material del Lote
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_detalle_reservas>-material
              IMPORTING
                output = les_items-material.

*            les_items-material = <lfs_detalle_reservas>-material.
*.....Centro Lote
            les_items-plant = <lfs_detalle_reservas>-plant.
*.....Almacen Lote
            les_items-stge_loc = <lfs_detalle_reservas>-stge_loc.
*.....Información de Lote
            les_items-batch = les_lotes_picking-lote.
*.....Cantidad Contada para el Lote

*....       Si la cantidad de las tablas de picking son menores o iguales a la cantidad que hay en el lote
*....       Asigno la Cantidad de las tablas de picking y resto esa cantidad al lote
            IF l_e_cant_cont <= les_lotes_picking-cantidad_uso.
              les_items-entry_qnt = l_e_cant_cont .     "Cantidad entregada efectivamente en UMV
              les_lotes_picking-cantidad_uso = les_lotes_picking-cantidad_uso - l_e_cant_cont.
              l_e_cant_cont = 0.
            ELSE.
*....       Si la cantidad de las tablas de picking es mayor a la cantidad que hay en el lote
*....       Asigno la Cantidad del lote , pongo cantidad de lote en 0 y resto a la cantidad de las tablas de picking
*...        la cantidad del lote
              les_items-entry_qnt = les_lotes_picking-cantidad_uso.     "Cantidad entregada efectivamente en UMV
              l_e_cant_cont = l_e_cant_cont - les_lotes_picking-cantidad_uso.
              les_lotes_picking-cantidad_uso = 0.
            ENDIF.

*            les_items-entry_qnt = <lfs_detalle_reservas>-entry_qnt.
*            les_items-entry_qnt = les_lotes_picking-cantidad_uso. "SO-138 LFY 23/04/2014


*.....Número de Orden
            les_items-orderid = cabeceras_ordenes-aufnr.
*.....Posicion del Material en la Orden
            les_items-order_itno = <lfs_detalle_reservas>-res_item.



*.... Destinatario
            IF ( les_resb-bwart EQ '591' OR les_resb-bwart EQ '592') AND zif_lgtica_documento~tipo_documento EQ 'RINV'.
*.....Función para Ajuste de Número de Posicion
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <lfs_detalle_reservas>-gr_rcpt
                IMPORTING
                  output = les_items-vendor.
            ENDIF.


*.....Número de Reserva
            les_items-reserv_no = <lfs_detalle_reservas>-reserv_no.
*.....Posición del Material en la reserva
            les_items-res_item = <lfs_detalle_reservas>-res_item.
*.....Tipo de Movimiento
            IF zif_lgtica_documento~tipo_documento EQ 'OPSG' .
*              les_items-withdrawn = 'X'.
              les_items-move_type = '261'.
            ELSEIF zif_lgtica_documento~tipo_documento EQ 'RINV'.
*              les_items-withdrawn = 'X'.
              les_items-move_type = '201'.
            ELSEIF zif_lgtica_documento~tipo_documento EQ 'OSIN'.
              les_items-move_type = '261'.
              les_items-withdrawn = ' '.
            ENDIF.
**.....Elimino de Tabla de Lotes el Registro Procesado
*            DELETE lti_lotes_picking WHERE  matnr = l_e_material AND
*                                            lote = les_lotes_picking-lote AND
*                                           ( aufnr = cabeceras_ordenes-aufnr OR
*                                             rsnum = cabecera_reservas-rsnum  ).

*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = les_items-material
              IMPORTING
                output = l_e_nmat.

*.... Consulto el registro de la z de ubicaciones (se modifica el where de la consulta Ajuste SO-128)
*{   REPLACE        ER3K900309                                        4
*\            SELECT SINGLE *
*\              INTO les_det_ubicacion
*\              FROM zmm_t_ubicacion
*\               WHERE material EQ l_e_nmat AND
*\                centro EQ les_items-plant AND
*\                almacen EQ les_items-stge_loc AND
*\*                 lote eq les_picking_lotes-lote  and
*\                ubicacion EQ les_lotes_picking-ubicacion_fi.
            SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                          NUMERO_DOC USUARIO  FECHA HORA
              INTO les_det_ubicacion
              FROM zmm_t_ubicacion
               WHERE material EQ l_e_nmat AND
                centro EQ les_items-plant AND
                almacen EQ les_items-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                ubicacion EQ les_lotes_picking-ubicacion_fi.
*}   REPLACE

*            SELECT SINGLE *
*              INTO les_det_ubicacion
*              FROM zmm_t_ubicacion
*               WHERE material EQ les_items-material AND
*                centro EQ les_items-plant AND
*                almacen EQ les_items-stge_loc AND
**                 lote eq les_picking_lotes-lote  and
*                ubicacion EQ les_lotes_picking-ubicacion_fi.

*   FIN SO-128 LFY 07/04/2015

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


*         INICIO SO-138 LFY 23/04/2014
              READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_lotes_picking-umc.
              IF sy-subrc EQ 0 .
*.... Para reserva de inventario,  Tabla RESB , cuando campo SHKZG= S debe sumar a inventario, por tanto debe sumar las cantidades en la tabla  ZMM_T_UBICACION,
*.... cuando SHKZG= H debe restar a inventario, por tanto debe restar las cantidades en la tabla
                IF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg EQ 'S' AND les_resb-bwart NE '301' ).
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                ELSEIF  ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-bwart EQ '301' ).
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                ELSEIF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg NE 'S' AND les_resb-bwart NE '301' ).
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                ELSE.
                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                ENDIF.

*                les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
              ELSE.
                CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                  EXPORTING
                    input    = les_lotes_picking-umc
                    language = sy-langu
                  IMPORTING
                    output   = les_lotes_picking-umc.
                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_lotes_picking-umc.
                IF sy-subrc EQ 0 .
*.... Para reserva de inventario,  Tabla RESB , cuando campo SHKZG= S debe sumar a inventario, por tanto debe sumar las cantidades en la tabla  ZMM_T_UBICACION,
*.... cuando SHKZG= H debe restar a inventario, por tanto debe restar las cantidades en la tabla
                  IF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg EQ 'S' AND les_resb-bwart NE '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSEIF  ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-bwart EQ '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSEIF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg NE 'S' AND les_resb-bwart NE '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSE.
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
*                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( <lfs_detalle_reservas>-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ENDIF.

                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ENDIF.
              ENDIF.

            ENDIF.




*            les_lotes_picking-cantidad_uso = 0.
*            les_lotes_picking-cantidad_uso = les_lotes_picking-cantidad_uso - <lfs_detalle_reservas>-entry_qnt.
            MODIFY lti_lotes_picking FROM les_lotes_picking INDEX l_e_index.
            APPEND les_items TO lti_items.
            CLEAR: les_items, les_lotes_picking.
            CONTINUE.
*          FIN SO-138 LFY 23/04/2014
          ENDLOOP.

            CLEAR:l_e_cont_cant_ser.

*.....Recorro tabl de seriales en caso de que existan posiciones con seriales
          IF l_e_control IS INITIAL .
*.....Función para Ajuste de Número de Posicion
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_detalle_reservas>-res_item
              IMPORTING
                output = l_e_pos.

*         INICIO SO-133 LFY 14/04/2015
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_e_material
              IMPORTING
                output = l_e_material.
*        FIN SO-133 LFY 14/04/2015

            LOOP AT lti_det_picking INTO les_det_picking
                                     WHERE  material EQ l_e_material AND
                                            posicion EQ l_e_pos AND p_confirm NE 'CT'.


*   INICIO SO-128 LFY 07/04/2015

*.....Utilizo función para Ajuste de Número de Material (Nuevo en el Ajuste SO-128)
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = les_det_picking-material
                IMPORTING
                  output = l_e_nmat.

*.... Consulto el registro de la z de ubicaciones (se modifica el where de la consulta Ajuste SO-128)
*{   REPLACE        ER3K900309                                        5
*\              SELECT SINGLE *
*\                INTO les_det_ubicacion
*\                FROM zmm_t_ubicacion
*\                 WHERE material EQ l_e_nmat AND
*\                  centro EQ <lfs_detalle_reservas>-plant AND
*\                  almacen EQ <lfs_detalle_reservas>-stge_loc AND
*\*                 lote eq les_picking_lotes-lote  and
*\                  ubicacion EQ les_det_picking-ubicacion_fi.
              SELECT SINGLE MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
                            NUMERO_DOC USUARIO  FECHA HORA
                INTO les_det_ubicacion
                FROM zmm_t_ubicacion
                 WHERE material EQ l_e_nmat AND
                  centro EQ <lfs_detalle_reservas>-plant AND
                  almacen EQ <lfs_detalle_reservas>-stge_loc AND
*                 lote eq les_picking_lotes-lote  and
                  ubicacion EQ les_det_picking-ubicacion_fi.
*}   REPLACE


*              SELECT SINGLE *
*                INTO les_det_ubicacion
*                FROM zmm_t_ubicacion
*                 WHERE material EQ les_det_picking-material AND
*                  centro EQ <lfs_detalle_reservas>-plant AND
*                  almacen EQ <lfs_detalle_reservas>-stge_loc AND
**                 lote eq les_picking_lotes-lote  and
*                  ubicacion EQ les_det_picking-ubicacion_fi.

*   FIN SO-128 LFY 07/04/2015

*.... Si hay registro descuento la cantidad consumida
              IF sy-subrc EQ 0.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = les_det_picking-material
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.

                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_det_picking-umc.
                IF sy-subrc EQ 0 .
*.... Para reserva de inventario,  Tabla RESB , cuando campo SHKZG= S debe sumar a inventario, por tanto debe sumar las cantidades en la tabla  ZMM_T_UBICACION,
*.... cuando SHKZG= H debe restar a inventario, por tanto debe restar las cantidades en la tabla
                  IF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg EQ 'S' AND les_resb-bwart NE '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( les_det_picking-cantcont * <lfs_detalle>-cantidad_ean ).  "SO-133 LFY 13/04/2015
*                   les_det_ubicacion-cantidad = les_det_ubicacion-cantidad + ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSEIF  ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-bwart EQ '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_det_picking-cantcont * <lfs_detalle>-cantidad_ean ). "SO-133 LFY 13/04/2015
*                  les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSEIF ( zif_lgtica_documento~tipo_documento EQ 'RINV' AND les_resb-shkzg NE 'S' AND les_resb-bwart NE '301' ).
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_det_picking-cantcont * <lfs_detalle>-cantidad_ean )."SO-133 LFY 13/04/2015
*                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_items-entry_qnt * <lfs_detalle>-cantidad_ean ).
                  ELSE.
                    les_det_ubicacion-cantidad = les_det_ubicacion-cantidad - ( les_det_picking-cantcont * <lfs_detalle>-cantidad_ean ).
                  ENDIF.

                  MODIFY zmm_t_ubicacion FROM  les_det_ubicacion.
                ENDIF.

              ENDIF.

              l_e_cant_pos =  l_e_cant_pos + les_det_picking-cantcont.
            ENDLOOP.

*.....Verifico que Encuentre Registro y que Tenga como minimo 1 Cantidad Contada
            IF l_e_cant_pos IS NOT INITIAL.
*.....Cantidad
              les_items-entry_qnt = l_e_cant_pos . "les_det_picking-cantcont.
*.....Material
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <lfs_detalle_reservas>-material
                IMPORTING
                  output = les_items-material.

*              les_items-material = <lfs_detalle_reservas>-material.
*.....Centro
              les_items-plant = <lfs_detalle_reservas>-plant.
*.....Almacen
              les_items-stge_loc = <lfs_detalle_reservas>-stge_loc.
*.....Lote
              les_items-batch = <lfs_detalle_reservas>-batch.
*.....Número de Orden
              les_items-orderid = cabeceras_ordenes-aufnr.
*.....Posicion del Material en la Orden
              les_items-order_itno = <lfs_detalle_reservas>-res_item.
*.....Número de Reserva
              les_items-reserv_no = <lfs_detalle_reservas>-reserv_no.
*.... Destinatario
*            les_items-GR_RCPT = <lfs_detalle_reservas>-GR_RCPT.
*.... Destinatario
              IF ( les_resb-bwart EQ '591' OR les_resb-bwart EQ '592') AND zif_lgtica_documento~tipo_documento EQ 'RINV'.
*.....Función para Ajuste de Número de Posicion
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <lfs_detalle_reservas>-gr_rcpt
                  IMPORTING
                    output = les_items-vendor.
              ENDIF.
*.....Posición del Material en la reserva
              les_items-res_item = <lfs_detalle_reservas>-res_item.
*.....Tipo de Movimiento
              IF zif_lgtica_documento~tipo_documento EQ 'OPSG' .
*                les_items-withdrawn = 'X'.
                les_items-move_type = '261'.
              ELSEIF zif_lgtica_documento~tipo_documento EQ 'RINV'.
*                les_items-withdrawn = 'X'.
                les_items-move_type = '201'.
              ELSEIF zif_lgtica_documento~tipo_documento EQ 'OSIN'.
                les_items-move_type = '261'.
*                les_items-withdrawn = ' '.
              ENDIF.
              APPEND les_items TO lti_items.
*.....Busqueda de Material Serializado
*{   REPLACE        ER3K900309                                        1
*\              LOOP AT lti_ser_picking INTO les_ser_picking
*\                                       WHERE matnr = l_e_material AND
*\                                             ( aufnr = cabeceras_ordenes-aufnr OR
*\                                           rsnum = cabecera_reservas-rsnum ) AND flag_ser NE 'X'.
*\
*\                les_serialnumber-matdoc_itm =  l_e_linea .
*\                les_serialnumber-serialno = les_ser_picking-serie.
*\                APPEND les_serialnumber TO lti_serialnumber.
*\
*\                les_ser_picking-flag_ser = 'X'.
*\
*\                APPEND les_ser_picking TO lti_picking_ser_aux.
*\*.....Elimino Material Serializado de tabla de Seriales
*\*                DELETE lti_ser_picking WHERE matnr = l_e_material AND
*\*                                             serie = les_ser_picking-serie.
*\                CLEAR: les_serialnumber, les_ser_picking.
*\              ENDLOOP.
              CLEAR:l_e_cont_cant_ser.

              LOOP AT lti_ser_picking INTO les_ser_picking
                                       WHERE matnr = l_e_material AND
                                             ( aufnr = cabeceras_ordenes-aufnr OR
                                           rsnum = cabecera_reservas-rsnum ) AND flag_ser NE 'X'.

                IF l_e_cant_pos = l_e_cont_cant_ser.
                   CONTINUE.
                ENDIF.

                les_ser_picking-flag_ser = 'X'.
*                MODIFY lti_ser_picking From les_ser_picking INDEX sy-tabix.

                les_serialnumber-matdoc_itm =  l_e_linea .
                les_serialnumber-serialno = les_ser_picking-serie.
                APPEND les_serialnumber TO lti_serialnumber.

                l_e_cont_cant_ser = l_e_cont_cant_ser + 1.
                APPEND les_ser_picking TO lti_picking_ser_aux.

*.....Elimino Material Serializado de tabla de Seriales
                DELETE lti_ser_picking WHERE matnr = l_e_material AND
                                             serie = les_ser_picking-serie.
                CLEAR: les_serialnumber, les_ser_picking.
              ENDLOOP.
*}   REPLACE
            ENDIF.
          ENDIF.
          CLEAR : les_items,  l_e_pos, l_e_control, l_e_cant_pos,l_e_cant_cont.
      ENDAT.
    ENDLOOP.
***********************        Fin Cambio de recorrido


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

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = cabeceras_ordenes-aufnr
          IMPORTING
            output = cabeceras_ordenes-aufnr.

*.....Existen Errores a Contabilizar la Orden
        IF lti_return[] IS NOT INITIAL.
          IF  cabecera_reservas IS NOT INITIAL .
            les_msg_picking-num_doc = cabecera_reservas-rsnum.
          ELSE.
            les_msg_picking-num_doc = cabeceras_ordenes-aufnr.
          ENDIF.
          LOOP AT  lti_return ASSIGNING <lfs_return>.
            les_msg_picking-msg = <lfs_return>-message.
            les_msg_picking-type_msg = <lfs_return>-type.
            APPEND les_msg_picking TO c_return.
            CLEAR : les_msg_picking.
          ENDLOOP.
*.....Deshacer Cambios realiazados en Bases de Datos
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*{   INSERT         ER3K900309                                        8
          ROLLBACK WORK.
*}   INSERT
        ELSE.
*.....Elimino Información de Tabla de Seriales y Lotes cuando el proceso es Exitoso
          IF lti_picking_lotes_aux[] IS NOT INITIAL .
            MODIFY zmm_t_pck_lot FROM TABLE lti_lotes_picking.
          ENDIF.
          IF lti_picking_ser_aux[] IS NOT INITIAL.
            MODIFY zmm_t_pck_ser FROM TABLE lti_picking_ser_aux.
          ENDIF.
          CONCATENATE 'Proceso Exitoso, Número de Documento Contabilización' space l_e_matnr space
           INTO l_e_msg RESPECTING BLANKS.
          IF  cabecera_reservas IS NOT INITIAL .
            les_msg_picking-num_doc = cabecera_reservas-rsnum.
          ELSE.
            les_msg_picking-num_doc = cabeceras_ordenes-aufnr.
          ENDIF.
          les_msg_picking-msg = l_e_msg.
          les_msg_picking-type_msg = 'S'.
          APPEND les_msg_picking TO c_return.
          CLEAR : les_msg_picking.
*.....Cerrar Picking
          lo_lgtica_picking->cerrar_picking(
            EXPORTING
              i_picknum = l_e_piknum
              i_usuario = lo_lgtica_picking->s_picking-usuario ).
        ENDIF.
      ELSE.
*.....Datos de Cabecera para la Contabilización de Proyectos
        les_header-pstng_date = sy-datum.
        les_header-doc_date = sy-datum.
        les_header-pr_uname = detalles_usuario-usuario.
        les_code-gm_code = l_e_tx.

*.....Datos de Entrega para el Consumo de Inventario (MIGO)
        LOOP AT zif_lgtica_documento~t_entregas INTO les_det_entrega
                                                WHERE entrega = i_entrega-entrega.

          DESCRIBE TABLE lti_items LINES l_e_linea.
          l_e_linea = l_e_linea + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = les_det_entrega-matnr
            IMPORTING
              output = les_items-material.
          l_e_material = les_items-material.
*          les_items-material = les_det_entrega-matnr.
          les_items-plant = les_det_entrega-werks.
          les_items-stge_loc = les_det_entrega-lgort.
          les_items-move_type = '291'.
          les_items-spec_stock = 'W'.
*.....Información de Lote
          les_items-batch = les_det_entrega-charg.
          les_items-customer = les_det_entrega-customer.
*.....Cantidad Contada
          les_items-entry_qnt = les_det_entrega-lfimg.
*.....Unidad de Medida
          les_items-entry_uom = les_det_entrega-vrkme.

*.....información de seriales
*.....Busqueda de Material Serializado
*{   REPLACE        ER3K900309                                        2
*\          LOOP AT lti_ser_picking INTO les_ser_picking
*\                                   WHERE matnr = l_e_material AND
*\                                         ( vbeln2 = i_entrega-entrega AND flag_ser EQ 'X' ).
*\
*\            les_serialnumber-matdoc_itm = l_e_linea.
*\            les_serialnumber-serialno = les_ser_picking-serie.
*\            APPEND les_serialnumber TO lti_serialnumber.
*\            CLEAR: les_serialnumber, les_ser_picking.
*\          ENDLOOP.
          CLEAR:l_e_cont_cant_ser.
          LOOP AT lti_ser_picking INTO les_ser_picking
                                   WHERE matnr = l_e_material AND
                                         ( vbeln2 = i_entrega-entrega AND flag_ser EQ 'X' ).
          IF les_items-entry_qnt = l_e_cont_cant_ser.
                   CONTINUE.
          ENDIF.
            l_e_cont_cant_ser = l_e_cont_cant_ser + 1.

            les_serialnumber-matdoc_itm = l_e_linea.
            les_serialnumber-serialno = les_ser_picking-serie.
            APPEND les_serialnumber TO lti_serialnumber.
            CLEAR: les_serialnumber, les_ser_picking.

*.....Elimino Material Serializado de tabla de Seriales
                DELETE lti_ser_picking WHERE matnr = l_e_material AND
                                             serie = les_ser_picking-serie.

          ENDLOOP.
          IF les_items-entry_qnt EQ 0.
            CONTINUE.
          ENDIF.
*}   REPLACE
*          IF les_det_entrega-sernr IS NOT INITIAL.
*            les_serialnumber-matdoc_itm = les_det_entrega-posnr.
*            les_serialnumber-serialno = les_det_entrega-sernr.
*            APPEND les_serialnumber TO lti_serialnumber.
*          ENDIF.
*.....Número Orden
          les_items-orderid = les_det_entrega-orderid.
*.....Posición Material Orden
          les_items-order_itno = les_det_entrega-posnr.

          APPEND les_items TO lti_items.
          CLEAR: les_items, les_det_picking, les_serialnumber.
        ENDLOOP.
*.....Bapi encargada de Realizar Contabilizar Documentos Que Generan Entrega (Proyectos)
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

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = cabeceras_ordenes-aufnr
          IMPORTING
            output = i_entrega-documento.

*.....Existen Errores a Contabilizar la Orden
        IF lti_return[] IS NOT INITIAL.
          LOOP AT  lti_return ASSIGNING <lfs_return>.
            les_msg_picking-num_doc = i_entrega-documento.
            les_msg_picking-msg = <lfs_return>-message.
            les_msg_picking-type_msg = <lfs_return>-type.
            APPEND les_msg_picking TO c_return.
            CLEAR : les_msg_picking.
          ENDLOOP.
*.....Deshacer Cambios realiazados en Bases de Datos
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*{   INSERT         ER3K900309                                        7
          ROLLBACK WORK.
*}   INSERT
        ELSE.
          CONCATENATE 'Consumo de Inventario Existoso, Número' space l_e_matnr space
          'Para el Pedido de Consignación Número ' space i_entrega-documento space
           INTO l_e_msg RESPECTING BLANKS.
          les_msg_picking-num_doc = i_entrega-documento.
          les_msg_picking-msg = l_e_msg.
          les_msg_picking-type_msg = 'S'.
          APPEND les_msg_picking TO c_return.
          CLEAR : les_msg_picking.
*.....Cerrar Picking
          lo_lgtica_picking->cerrar_picking(
            EXPORTING
              i_picknum = l_e_piknum
              i_usuario = lo_lgtica_picking->s_picking-usuario ).
        ENDIF.
      ENDIF.
    ELSE.
*.....Número de Pedido para Ordenes de Servicio en Proyectos
      IF les_cab_picking-vbeln IS NOT INITIAL.
        les_msg_picking-num_doc = les_cab_picking-vbeln.
*.....Número de Reserva
      ELSEIF les_cab_picking-aufnr IS NOT INITIAL AND les_cab_picking-rsnum IS NOT INITIAL.
        les_msg_picking-num_doc = les_cab_picking-rsnum.
*.....Número de Orden
      ELSE.
        les_msg_picking-num_doc = les_cab_picking-aufnr.
      ENDIF.
      CONCATENATE 'El Documento' space les_msg_picking-num_doc space
      'Esta Contabilizado y/o No esta Registrado en la Tabla de Picking' space INTO l_e_msg RESPECTING BLANKS.
      les_msg_picking-msg = l_e_msg.
      les_msg_picking-type_msg = 'E'.
      APPEND les_msg_picking TO c_return.
      CLEAR : les_msg_picking.
    ENDIF.
*.....Limpieza de Variables
    CLEAR : les_header, les_code, les_headret, l_e_matnr, l_e_year, lti_items,
            lti_serialnumber, lti_return.
  ENDIF.
ENDMETHOD.


METHOD crear_transporte.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado del Llamado al Metodo para Crear
*                Ordenes de Transporte para las Entregas
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 29.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 29.07.2014     ER6K907091    Marco A. Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
*.....Variable para Msg
          l_e_msg TYPE string,
*.....Estructura con Mensajes
          les_bapiret1 TYPE bapiret1,
*.....Mensajes de Respuesta
          les_return TYPE zesd_msg_picking,
*.....Número de Transporte
          l_e_tanum TYPE tanum.

  CALL METHOD me->create_to_for_delivery
    EXPORTING
      i_lgnum    = i_entrega-lgnum
      i_entrega  = i_entrega-entrega
    IMPORTING
      e_tanum    = l_e_tanum
      e_bapiret1 = les_bapiret1.

  IF les_bapiret1 IS NOT INITIAL  .
    CONCATENATE 'Entrega Número:' space i_entrega-entrega space les_bapiret1-message space
    INTO l_e_msg RESPECTING BLANKS.
    les_return-num_doc = i_entrega-documento.
    les_return-msg = l_e_msg.
    les_return-type_msg = les_bapiret1-type.
    APPEND les_return TO c_return.
    CLEAR: les_return, les_bapiret1, l_e_msg.
  ELSE.
    e_transporte = l_e_tanum.
  ENDIF.

  CLEAR : l_e_tanum.
ENDMETHOD.


METHOD create_to_for_delivery.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Crea Orden de Transporte
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 18.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 18.07.2014    ER6K907009   Marco Suarez        Creación
*-------------------------------------------------------------------------------*


DATA:
        les_bapiret1 TYPE bapiret1,
        l_e_usuario TYPE sy-uname,
        l_e_tanum TYPE tanum.

*.....Dato de Usuario
      l_e_usuario = detalles_usuario-usuario.

*.....Función para Crear la Orden de Transporte
    CALL FUNCTION 'L_TO_CREATE_DN'
      EXPORTING
        i_lgnum                    = i_lgnum
        i_vbeln                    = i_entrega
        i_commit_work              = ' '
        i_bname                    = l_e_usuario  "sy-uname
      IMPORTING
        e_tanum                    = e_tanum
      EXCEPTIONS
        foreign_lock               = 1
        dn_completed               = 2
        partial_delivery_forbidden = 3
        xfeld_wrong                = 4
        ldest_wrong                = 5
        drukz_wrong                = 6
        dn_wrong                   = 7
        squit_forbidden            = 8
        no_to_created              = 9
        teilk_wrong                = 10
        update_without_commit      = 11
        no_authority               = 12
        no_picking_allowed         = 13
        dn_hu_not_choosable        = 14
        input_error                = 15
        OTHERS                     = 16.
    IF sy-subrc <> 0.
*.....Función para Obtener la Descripcion del Error de la Función
      CALL FUNCTION 'BALW_BAPIRETURN_GET1'
        EXPORTING
          type       = sy-msgty
          cl         = sy-msgid
          number     = sy-msgno
        IMPORTING
          bapireturn = e_bapiret1.
    ENDIF.

ENDMETHOD.


METHOD get_ajustar_documento.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Ajuste de un Número de Documento
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 27.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_documento
    IMPORTING
      output = r_documento.

ENDMETHOD.


METHOD get_detalles_usuario.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodos para Obtener Detalles de Usuario
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 27.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Función que retorna para Un Usuario el USUARIO,COD_IMPRESORA,CENTRO,ALMACEN
  CALL FUNCTION 'Z_SD_CONSUL_DAT_USER_MOV'
    EXPORTING
      usuario     = i_usuario
    IMPORTING
      usuario_det = e_det_usuario
    TABLES
      t_mensajes  = c_mensajes.

ENDMETHOD.


method GET_PICKNUM.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Cantidades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  constants: lc_proceso type c length 20 value 'PICKING'.
  constants: lc_subproc type c length 4 value  'SUBC'.

*.....Tabla para Busqueda de Documentos Picking
  data:    lti_pick type table of zmm_t_picking,
*.....Tabla para Busqueda de Posiciones de Documentos Picking
           lti_pick_dt type table of zmm_t_picking_dt.

*.....Estructura para Documentos picking
  data:    les_pick type  zmm_t_picking,
*.....Estructura para Posiciones de Documentos Picking
           les_pick_dt type  zmm_t_picking_dt.

  "Declarar variables
  data: wobjeto      type tnro-object    value 'ZEDPICKING', " Nombre del SNRO
        wnorange     type inri-nrrangenr value '01',        "Número de rango,
        wsubobj      type inri-subobject value space,       "Subobject
        w_doc_number type char10.

  data:  p_doc_number type char18. " Variable para almacenar el número generado

*..... Consulto si ya se ha creado un registro con la entrega y documento de ventas para obtener el numpick

*{   REPLACE        ER3K900279                                        1
*\  select single *
  select single MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA
USUARIO  UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO

*}   REPLACE
    into  les_pick
    from zmm_t_picking
    where vbeln2 eq entrega.

*...  Obtengo el consecutivo generado por SAP para el Rango ZEDPICKING
  if sy-subrc ne 0.
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

    picknum = p_doc_number.
*    les_pick-picknum = p_doc_number. " Número generado por SAP.
*    les_pick-proceso = proceso. " proceso  del documento
*    les_pick-subproceso = subproceso. "subproceso del documento
*    les_pick-vbeln = vbeln. " documento de ventas
**les_pick-EBELN = ''.
**les_pick-AGLUTINADOR = ''.
*    les_pick-vbeln2 = entrega. " entrega
*    les_pick-fecha = sy-datum. "fecha de creación
*    les_pick-hora = sy-uzeit. " hora de creación
*    les_pick-usuario = usuario. " usuario movil creación
*    les_pick-uname = sy-uname. " usuario sap creación
*    les_pick-femod = sy-datum. " fecha de modificación
*    les_pick-homod = sy-uzeit. " hora de modificación
*    les_pick-usrmod = usuario. " usuario movil modificación
*    les_pick-unamod = sy-uname. " usuario sap modificación
*    les_pick-estado = 'PENDIENTE'. " estado

  else.
    picknum = les_pick-picknum.
*    les_pick-proceso = proceso.
*    les_pick-subproceso = subproceso.
*    les_pick-femod = sy-datum.
*    les_pick-homod = sy-uzeit.
*    les_pick-usrmod = usuario.
*    les_pick-unamod = sy-uname.
*    les_pick-estado = estado.
  endif.

*  modify zmm_t_picking from les_pick.

endmethod.


METHOD post_goods_issue.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Contabiliza Salida de Mercancias
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 18.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 18.07.2014    ER6K907009   Marco Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
      lti_return TYPE bapiret2_tab,
      les_bapiret1 TYPE bapiret1,

*......Estructuras para Bapi
          les_header_data TYPE bapiobdlvhdrcon,
          les_header_control TYPE bapiobdlvhdrctrlcon,
          l_e_delivery TYPE vbeln_vl,
          les_header_data_spl TYPE /spe/bapiobdlvhdrconf,
          les_header_control_spl TYPE /spe/bapiobdlvhdrctrlcon.

*.....Field Symbols para Tabla de Retorno
  FIELD-SYMBOLS: <lfs_return> TYPE bapiret2.

*.....Lenado de Campos
  les_header_data-deliv_numb = i_entrega.
  les_header_control-deliv_numb = i_entrega.
  les_header_control-post_gi_flg = 'X'.
  l_e_delivery = i_entrega.
  les_header_data_spl-deliv_numb = i_entrega.
  les_header_control_spl-deliv_numb = i_entrega.

*.....Bapi para Contabilizar o dar Salida de Mercancias
  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
    EXPORTING
      header_data        = les_header_data
      header_control     = les_header_control
      delivery           = l_e_delivery
      header_data_spl    = les_header_data_spl
      header_control_spl = les_header_control_spl
    TABLES
      return             = lti_return.

*.....Verifico Resultado de Proceso, si la Tabla lti_return esta Vacia, significa que el Proceso
*.....fue Exitoso.
  IF lti_return[] IS NOT INITIAL .
*.....Recorro Tabla de Mensajes de Error
    LOOP AT lti_return ASSIGNING <lfs_return>.
*.....Función para la Lectura de Clases de Mensajes retornado por la Función de
*.....Contabilización
      CALL FUNCTION 'BALW_BAPIRETURN_GET1'
        EXPORTING
          type       = <lfs_return>-type
          cl         = <lfs_return>-id
          number     = <lfs_return>-number
        IMPORTING
          bapireturn = les_bapiret1.
*.....Inserto en Tabla de Errores
      APPEND les_bapiret1 TO c_bapiret1.
    ENDLOOP.
  ENDIF.
*.....Limpieza de Variables
    clear : les_bapiret1, lti_return, les_header_data, les_header_control,
            l_e_delivery, les_header_data_spl, les_header_control_spl.
ENDMETHOD.


METHOD set_cab_cant1.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Cantidades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_subproc TYPE c LENGTH 4 VALUE  'SUBC'.

*.....Tabla para Busqueda de Documentos Picking
  DATA:    lti_pick TYPE TABLE OF zmm_t_picking,
*.....Tabla para Busqueda de Posiciones de Documentos Picking
           lti_pick_dt TYPE TABLE OF zmm_t_picking_dt.

*.....Estructura para Documentos picking
  DATA:    les_pick TYPE  zmm_t_picking,
*.....Estructura para Posiciones de Documentos Picking
           les_pick_dt TYPE  zmm_t_picking_dt.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPICKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado

*..... Consulto si ya se ha creado un registro con la entrega y documento de ventas para obtener el numpick

*{   REPLACE        ER3K900279                                        1
*\  SELECT SINGLE *
  SELECT SINGLE MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM  FECHA HORA USUARIO  UNAME FEMOD HOMOD
*}   REPLACE
    INTO  les_pick
    FROM zmm_t_picking
    WHERE vbeln EQ vbeln
    AND vbeln2 EQ entrega.

*...  Obtengo el consecutivo generado por SAP para el Rango ZEDPICKING
  IF sy-subrc NE 0.
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

    les_pick-picknum = p_doc_number. " Número generado por SAP.
    les_pick-proceso = proceso. " proceso  del documento
    les_pick-subproceso = subproceso. "subproceso del documento
    les_pick-vbeln = vbeln. " documento de ventas
*les_pick-EBELN = ''.
*les_pick-AGLUTINADOR = ''.
    les_pick-vbeln2 = entrega. " entrega
    les_pick-fecha = sy-datum. "fecha de creación
    les_pick-hora = sy-uzeit. " hora de creación
    les_pick-usuario = usuario. " usuario movil creación
    les_pick-uname = sy-uname. " usuario sap creación
    les_pick-femod = sy-datum. " fecha de modificación
    les_pick-homod = sy-uzeit. " hora de modificación
    les_pick-usrmod = usuario. " usuario movil modificación
    les_pick-unamod = sy-uname. " usuario sap modificación
    les_pick-estado = 'PENDIENTE'. " estado

  ELSE.
    les_pick-proceso = proceso.
    les_pick-subproceso = subproceso.
    les_pick-femod = sy-datum.
    les_pick-homod = sy-uzeit.
    les_pick-usrmod = usuario.
    les_pick-unamod = sy-uname.
    les_pick-estado = estado.
  ENDIF.

  MODIFY zmm_t_picking FROM les_pick.

ENDMETHOD.


method SET_CAB_CANT2.

*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Cantidades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_subproc TYPE c LENGTH 4 VALUE  'SUBC'.

*.....Tabla para Busqueda de Documentos Picking
  DATA:    lti_pick TYPE TABLE OF zmm_t_picking,
*.....Tabla para Busqueda de Posiciones de Documentos Picking
           lti_pick_dt TYPE TABLE OF zmm_t_picking_dt.

*.....Estructura para Documentos picking
  DATA:    les_pick TYPE  zmm_t_picking,
*.....Estructura para Posiciones de Documentos Picking
           les_pick_dt TYPE  zmm_t_picking_dt.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPICKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado

*..... Consulto si ya se ha creado un registro con la entrega y documento de ventas para obtener el numpick

*{   REPLACE        ER3K900279                                        1
*\  SELECT SINGLE *
  SELECT SINGLE MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM  FECHA HORA USUARIO  UNAME FEMOD HOMOD
*}   REPLACE
    INTO  les_pick
    FROM zmm_t_picking
    WHERE ebeln EQ ebeln
    AND vbeln2 EQ entrega.

*...  Obtengo el consecutivo generado por SAP para el Rango ZEDPICKING
  IF sy-subrc NE 0.
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

    les_pick-picknum = p_doc_number. " Número generado por SAP.
    les_pick-proceso = proceso. " proceso  del documento
    les_pick-subproceso = subproceso. "subproceso del documento
*    les_pick-vbeln = vbeln. " documento de ventas
   les_pick-EBELN = EBELN.
*les_pick-AGLUTINADOR = ''.
    les_pick-vbeln2 = entrega. " entrega
    les_pick-fecha = sy-datum. "fecha de creación
    les_pick-hora = sy-uzeit. " hora de creación
    les_pick-usuario = usuario. " usuario movil creación
    les_pick-uname = sy-uname. " usuario sap creación
    les_pick-femod = sy-datum. " fecha de modificación
    les_pick-homod = sy-uzeit. " hora de modificación
    les_pick-usrmod = usuario. " usuario movil modificación
    les_pick-unamod = sy-uname. " usuario sap modificación
    les_pick-estado = 'PENDIENTE'. " estado

  ELSE.
    les_pick-proceso = proceso.
    les_pick-subproceso = subproceso.
    les_pick-femod = sy-datum.
    les_pick-homod = sy-uzeit.
    les_pick-usrmod = usuario.
    les_pick-unamod = sy-uname.
    les_pick-estado = estado.
  ENDIF.

  MODIFY zmm_t_picking FROM les_pick.


endmethod.


method SET_CAB_CANT3.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Cantidades
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_subproc TYPE c LENGTH 4 VALUE  'SUBC'.

*.....Tabla para Busqueda de Documentos Picking
  DATA:    lti_pick TYPE TABLE OF zmm_t_picking,
*.....Tabla para Busqueda de Posiciones de Documentos Picking
           lti_pick_dt TYPE TABLE OF zmm_t_picking_dt.

*.....Estructura para Documentos picking
  DATA:    les_pick TYPE  zmm_t_picking,
*.....Estructura para Posiciones de Documentos Picking
           les_pick_dt TYPE  zmm_t_picking_dt.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPICKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado

*..... Consulto si ya se ha creado un registro con la entrega y documento de ventas para obtener el numpick

*{   REPLACE        ER3K900279                                        1
*\  SELECT SINGLE *
  SELECT SINGLE MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM  FECHA HORA USUARIO  UNAME FEMOD HOMOD
*}   REPLACE
    INTO  les_pick
    FROM zmm_t_picking
    WHERE Aglutinador EQ Aglutinador
    AND vbeln2 EQ entrega.

*...  Obtengo el consecutivo generado por SAP para el Rango ZEDPICKING
  IF sy-subrc NE 0.
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

    les_pick-picknum = p_doc_number. " Número generado por SAP.
    les_pick-proceso = proceso. " proceso  del documento
    les_pick-subproceso = subproceso. "subproceso del documento
*    les_pick-vbeln = vbeln. " documento de ventas
*les_pick-EBELN = ''.
    les_pick-AGLUTINADOR = Aglutinador.
*    les_pick-vbeln2 = entrega. " entrega
    les_pick-fecha = sy-datum. "fecha de creación
    les_pick-hora = sy-uzeit. " hora de creación
    les_pick-usuario = usuario. " usuario movil creación
    les_pick-uname = sy-uname. " usuario sap creación
    les_pick-femod = sy-datum. " fecha de modificación
    les_pick-homod = sy-uzeit. " hora de modificación
    les_pick-usrmod = usuario. " usuario movil modificación
    les_pick-unamod = sy-uname. " usuario sap modificación
    les_pick-estado = 'PENDIENTE'. " estado

  ELSE.
    les_pick-proceso = proceso.
    les_pick-subproceso = subproceso.
    les_pick-femod = sy-datum.
    les_pick-homod = sy-uzeit.
    les_pick-usrmod = usuario.
    les_pick-unamod = sy-uname.
    les_pick-estado = estado.
  ENDIF.

  MODIFY zmm_t_picking FROM les_pick.

endmethod.


method ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS.
endmethod.


method ZIF_LGTICA_DOCUMENTO~GENERAR_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Generar información de tablas packing para entregas
* Autor Prog.  :
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.... Referencia a la interfaz packing
DATA: lo_pack TYPE REF TO zif_lgtica_packing.
* Cast
      lo_pack = me.
*.... Crea Registros en Tabla de Cabeceras y Posiciones Packing
      CALL METHOD zcl_lgtica_packing=>create_by_pakble_ref
        EXPORTING
          i_ref_packable = lo_pack.

*.....Actualizo Base de Datos
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

endmethod.


method ZIF_LGTICA_DOCUMENTO~GET_CABECERA.

endmethod.


  method ZIF_LGTICA_DOCUMENTO~GET_CABECERA_PACKING.
  endmethod.


METHOD zif_lgtica_documento~get_cant_agl.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Ordenes de Prod. Sin Aglutinador
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 14.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*


  TYPES: BEGIN OF ltp_zppt_0001,
  id  TYPE  zppde_0001,
  nmint TYPE  zppde_0002,
  tipo  TYPE  zppde_0003,
  nm_ref  TYPE  zppde_0004,
  nm_ref_p  TYPE  zppde_0005,
  nm_ref_h  TYPE  zppde_0006,
  ref_pro TYPE  matnr,
  color_a TYPE  zppe_0002_001,
  color_b TYPE  zppe_0002_001,
  espejo  TYPE  zppe_0003_001,
  nic TYPE  zppe_0001_001,
  obser TYPE  zppe_0001_002,
  END OF ltp_zppt_0001.

*.....Typo para Datos de Ordenes
  TYPES: BEGIN OF ltp_afko,
         aufnr TYPE aufnr,        "Número de orden
         gamng  TYPE gamng,        " Cantidad teorica
         gmein  TYPE meins,        " Unidad de medida Base
         plnbez TYPE matnr,       "Tipo de orden
         rsnum  TYPE rsnum,
       END OF ltp_afko.

  DATA :
*..... Tabla interna con datos de la tabla de el aglutinador y la asociación de las ordenes
        lti_zppt_0001 TYPE TABLE OF ltp_zppt_0001,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
        lti_detalle TYPE TABLE OF zesd_eanmat,
        lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Tabla Interna con datos Basicos de Orden
        lti_afko TYPE TABLE OF ltp_afko,
*.....Variable Estructura para Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Mensaje
        l_e_msg TYPE string,
*.....Validador
        l_e_val TYPE i,
*..... Flag de Serial
        l_e_flag_ser TYPE string,
*{   INSERT         ER3K900309                                        3
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*}   INSERT
*.....Contador
        l_e_cont TYPE i,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string,
*.... Aglutinador
        l_e_nmint TYPE zppde_0002.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS:
                 <lfs_afko> TYPE ltp_afko,
                 <lfs_detalle> TYPE zesd_eanmat,
                 <lfs_mensajes> TYPE zesd_msg_picking,
                 <lfs_zppt_001> TYPE ltp_zppt_0001.


*.....Ajusto Número de Documento
  MOVE i_nmint TO l_e_nmint.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_nmint
    IMPORTING
      output = l_e_nmint.


*.... Busqueda de aglutinador en la tabla   Agrupador Ordenes de producción
*{   REPLACE        ER3K900309                                        1
*\  SELECT * FROM zppt_0001
*\    INTO CORRESPONDING FIELDS OF TABLE lti_zppt_0001
*\    WHERE nmint EQ l_e_nmint
*\    ORDER BY nm_ref_p.
  SELECT ID NMINT  TIPO NM_REF NM_REF_P NM_REF_H REF_PRO COLOR_A COLOR_B ESPEJO
         NIC
    FROM zppt_0001
    INTO TABLE lti_zppt_0001
    WHERE nmint EQ l_e_nmint
    ORDER BY nm_ref_p.
*}   REPLACE
*.... Validamos que se hayan encontrado el aglutinador
  IF sy-subrc EQ 0 .
*.....Busqueda Ordenes de Ser. para El Usuario Según el Centro y Documento Origen que tengan Asignados
    SELECT aufnr gamng gmein plnbez rsnum
             FROM afko
                INTO TABLE lti_afko
                  FOR ALL ENTRIES IN lti_zppt_0001
                  WHERE aufnr EQ lti_zppt_0001-nm_ref_p.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
    LOOP AT lti_afko ASSIGNING <lfs_afko>.
      les_mara-matnr = <lfs_afko>-plnbez.
      APPEND les_mara TO lti_mara.

*.....Función para Obtener EANs de un Material
      CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
        TABLES
          t_mara = lti_mara.
    ENDLOOP.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*.... Recorro las ordenes y agrego los materiales del aglutinador
    LOOP AT lti_afko ASSIGNING <lfs_afko>.
      CLEAR:les_det_pos,lti_detalle,lti_mensajes,l_e_flag_ser.

*.....Función para Obtener EANs de un Material
      CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
        EXPORTING
          matnr      = <lfs_afko>-plnbez
        TABLES
          t_detalle  = lti_detalle
          t_mensajes = lti_mensajes.
*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores en los EAN´s del Material
      READ TABLE lti_mensajes  INDEX 1 ASSIGNING  <lfs_mensajes>.
      IF sy-subrc EQ 0 .
        les_msg-num_doc = <lfs_mensajes>-num_doc.
        les_msg-msg = <lfs_mensajes>-msg.
        les_msg-type_msg = <lfs_mensajes>-msg.
        APPEND les_msg TO c_mensajes.
      ELSE.
*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
        IF lti_detalle IS NOT INITIAL.
          LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*.... Orden padre
            les_det_pos-num_entrega = <lfs_afko>-aufnr.
*.... Orden padre
            les_det_pos-nm_ref_p = <lfs_afko>-aufnr.
*.... Aglutinador
            les_det_pos-aglutinador = i_nmint.
*.... Material
            les_det_pos-material = <lfs_afko>-plnbez.
*.... Cantidad Teorica
            les_det_pos-cant_por_pick = <lfs_afko>-gamng.
*.... Unidad de medida
            les_det_pos-uni_med_doc =  <lfs_afko>-gmein.


*                      les_det_pos-material = <lfs_vbap>-matnr.
*                      les_det_pos-desc_material = <lfs_vbap>-arktx.
*                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
            les_det_pos-ubic_fija = ' '.
            les_det_pos-ubic_tmp =  ' '.
            les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input          = <lfs_afko>-gmein
                language       = 'S'
              IMPORTING
                output         = l_e_meinh
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
            IF l_e_meinh EQ '004'.
              les_det_pos-uni_med_doc = <lfs_afko>-gmein.
            ELSE.
              les_det_pos-uni_med_doc = l_e_meinh.
            ENDIF.
*                      les_det_pos-seriales = <lfs_vbap>-seriales.
*                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
            les_det_pos-ean = <lfs_detalle>-ean.
            les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
            les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*{   DELETE         ER3K900337                                        4
*\            LOOP AT lti_zppt_0001 ASSIGNING <lfs_zppt_001> WHERE nm_ref_p = <lfs_afko>-aufnr AND nic IS NOT INITIAL.
*\              l_e_flag_ser = 'X'.
*\              les_det_pos-seriales = <lfs_zppt_001>-nic.
*\              APPEND les_det_pos TO c_reg_posiciones.
*\            ENDLOOP.
*}   DELETE
*{   INSERT         ER3K900309                                        2
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = les_det_pos-material.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = ''
                  lgort     = ''
                  username  = i_usuario
                TABLES
                  t_return  = lti_return
                  t_serno   = lti_serno     "Serie para el Material
                  t_charg   = lti_charg     "Lote para el Material
                  t_estatin = lti_estatin
                  t_matnr   = lti_matnr.

*.....Verifico que el Material Tenga Número de Serie
                IF lti_serno IS NOT INITIAL.
                LOOP AT lti_zppt_0001 ASSIGNING <lfs_zppt_001> WHERE nm_ref_p = <lfs_afko>-aufnr and nm_ref_h = <lfs_afko>-aufnr AND nic IS NOT INITIAL.
                   l_e_flag_ser = 'X'.
                    les_det_pos-seriales = <lfs_zppt_001>-nic.
                     APPEND les_det_pos TO c_reg_posiciones.
                      ENDLOOP.

                ENDIF.

*}   INSERT

            IF l_e_flag_ser IS INITIAL.
              APPEND les_det_pos TO c_reg_posiciones.
            ENDIF.
            CLEAR les_det_pos.
          ENDLOOP.
        ELSE.
          les_msg-num_doc = ' '.
          CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_afko>-plnbez
          INTO l_e_desc_error RESPECTING BLANKS.
          les_msg-msg = l_e_desc_error.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.

  ENDIF.

ENDMETHOD.


method ZIF_LGTICA_DOCUMENTO~GET_DETALLE.
endmethod.


  method ZIF_LGTICA_DOCUMENTO~GET_DETALLE_PACKING.
  endmethod.


METHOD ZIF_LGTICA_DOCUMENTO~GET_TIPO_DOCUMENTO.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado de Obtener el Tipo de Documento
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 26.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 26.06.2014    ER6K906849    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Retorno el Tipo de Documento

  r_tipo_documento = me->zif_lgtica_documento~tipo_documento.

ENDMETHOD.


method ZIF_LGTICA_DOCUMENTO~GET_UBICACION_SUGERIDA.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Obtener la Ubicación sugerida del Documentos para Subcontratados
* Autor Prog.  :
* Fecha Creac. : 20.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
*... Tabla interna para consultar la tabla de ubicaciones
data: ti_zmm_t_ubicacion type standard table of zmm_t_ubicacion.
*...
data: les_ubicacion type  zmm_t_ubicacion.

data: les_ubicacion_suge type  ZESD_UBIC_SUG.
*... Detalle Posiciones para un Documento
data: c_det_posiciones type  zttsd_det_ref.
*... Números de Serie por material
data:c_seriales  type zttsd_str_nro_serie.
*... Números de Lote x Material
data: c_lotes   type zttsd_str_nro_lote.
**... Campos Errores en Procesamiento Cabecera
data: les_msg type zesd_msg_picking.
*DATA: C_MENSAJES type ZTTSD_MSG_PICKING.
*.....Variable Texto Error
data:          l_e_desc_error type string.

*.....Variable Estructura para Tabla de Mensajes
*data:         les_msg type zesd_msg_picking.

**.....Llamado a Metodo para Obtener los Detalles de Usuario
*    call method me->get_detalles_usuario
*      exporting
*        i_usuario     = i_usuario
*      importing
*        e_det_usuario = detalles_usuario
*      changing
*        c_mensajes    = c_mensajes.
*
*READ TABLE c_mensajes into les_msg WITH KEY TYPE_MSG = 'E'.
if detalles_usuario is initial.
c_mensajes = mensajes.
else.
*{   REPLACE        ER3K900279                                        1
*\refresh c_mensajes.
*\*... Consulto el detalle del documento para traer los materiales
*\call method me->zif_lgtica_documento~get_detalle
*\  exporting
*\    i_documento_origen = i_documento_origen
*\    i_tipo_documento   = i_tipo_documento
*\    i_clase_documento  = i_clase_documento
*\    i_usuario          = i_usuario
*\  changing
*\    c_det_posiciones   = c_det_posiciones
*\    c_seriales         = c_seriales
*\    c_lotes            = c_lotes
*\    c_mensajes         = c_mensajes
*\    .
*\*... ordeno por numero de documento y material
*\sort c_det_posiciones by num_entrega material.
*\*... Borro los materilales duplicados.
*\delete adjacent duplicates from c_det_posiciones comparing num_entrega material.
*\
*\*... Valido que hayan datos
*\  if not c_det_posiciones[] is initial.
*\*... Consulto las ubicaciones para todos los materiales
*\        select *
*\        into table ti_zmm_t_ubicacion
*\        from zmm_t_ubicacion
*\        for all entries in c_det_posiciones
*\        where material eq c_det_posiciones-material
*\          and centro   eq detalles_usuario-centro
*\          and almacen  eq detalles_usuario-almacen.
*\*... Verifico que haya traído datos la consulta
*\       if sy-subrc eq 0.
*\*... Recorro las ubicaciones
*\        loop at ti_zmm_t_ubicacion into les_ubicacion.
*\*...      Limpio la variable
*\          clear:les_ubicacion_suge.
*\*...  Asigno los valores
*\         les_ubicacion_suge-documento = i_documento_origen.
*\         les_ubicacion_suge-material  = les_ubicacion-material.
*\         les_ubicacion_suge-ubicacion = les_ubicacion-ubicacion.
*\         les_ubicacion_suge-cantidad = les_ubicacion-cantidad.
*\
*\*...    Valido si es una ubicación principal o secundaria
*\         if les_ubicacion-ubic_default eq 'X'.
*\           les_ubicacion_suge-flag_ubic = 'D'.
*\         else.
*\           les_ubicacion_suge-flag_ubic = 'S'.
*\         endif.
*\
*\         append les_ubicacion_suge to c_ubic_sugeridas.
*\        endloop.
*\        sort c_ubic_sugeridas by material.
*\      else.
*\                    les_msg-num_doc = i_documento_origen.
*\                    concatenate 'No existen Ubicaciones para el documento' i_documento_origen
*\                    into l_e_desc_error separated by space.
*\                    les_msg-msg = l_e_desc_error.
*\                    les_msg-type_msg = 'E'.
*\                    append les_msg to c_mensajes.
*\      endif.
*... Valido que hayan datos
  if not i_t_matnr[] is initial.
*... Consulto las ubicaciones para todos los materiales
        select MANDT SOCIEDAD CENTRO ALMACEN MATERIAL UBICACION UBIC_DEFAULT LOTE CANTIDAD UNIDAD_MED
               NUMERO_DOC USUARIO  FECHA HORA
        into table ti_zmm_t_ubicacion
        from zmm_t_ubicacion
        for all entries in i_t_matnr
        where material eq i_t_matnr-matnr
          and centro   eq detalles_usuario-centro
          and almacen  eq detalles_usuario-almacen.
*... Verifico que haya traído datos la consulta
       if sy-subrc eq 0.
*... Recorro las ubicaciones
        loop at ti_zmm_t_ubicacion into les_ubicacion.
*...      Limpio la variable
          clear:les_ubicacion_suge.
*...  Asigno los valores
         les_ubicacion_suge-documento = i_documento_origen.
         les_ubicacion_suge-material  = les_ubicacion-material.
         les_ubicacion_suge-ubicacion = les_ubicacion-ubicacion.
         les_ubicacion_suge-cantidad = les_ubicacion-cantidad.

*...    Valido si es una ubicación principal o secundaria
         if les_ubicacion-ubic_default eq 'X'.
           les_ubicacion_suge-flag_ubic = 'D'.
         else.
           les_ubicacion_suge-flag_ubic = 'S'.
         endif.

         append les_ubicacion_suge to c_ubic_sugeridas.
        endloop.
        sort c_ubic_sugeridas by material.
      else.
                    les_msg-num_doc = i_documento_origen.
                    concatenate 'No existen Ubicaciones para el documento' i_documento_origen
                    into l_e_desc_error separated by space.
                    les_msg-msg = l_e_desc_error.
                    les_msg-type_msg = 'E'.
                    append les_msg to c_mensajes.
      endif.

*}   REPLACE
  endif.

endif.
endmethod.


  method ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT.
  endmethod.


method ZIF_LGTICA_DOCUMENTO~REG_CANTIDADES.
endmethod.


METHOD zif_lgtica_documento~set_aglutinador.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo para Setiar Atributos de Cabecera y Detalle de Ordenes
*                de Servicio y de Producción Aglutinador
* Autor Prog.  :
* Fecha Creac. : 13.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*{   INSERT         ER3K900309                                        1

TYPES: BEGIN OF ltp_resb,
 rsnum    type resb-rsnum,
 rspos    type resb-rspos,
 erfme    type resb-erfme,
 erfmg    type resb-erfmg,
 meins    type resb-meins,
 bdmng    type resb-bdmng,
 bwart    type resb-bwart,
  END OF ltp_resb.

*}   INSERT
  TYPES: BEGIN OF ltp_zppt_0001,
   id  TYPE  zppde_0001,
   nmint TYPE  zppde_0002,
   tipo  TYPE  zppde_0003,
   nm_ref  TYPE  zppde_0004,
   nm_ref_p  TYPE  zppde_0005,
   nm_ref_h  TYPE  zppde_0006,
   ref_pro TYPE  matnr,
   color_a TYPE  zppe_0002_001,
   color_b TYPE  zppe_0002_001,
   espejo  TYPE  zppe_0003_001,
   nic TYPE  zppe_0001_001,
   obser TYPE  zppe_0001_002,
   END OF ltp_zppt_0001.
*.....Typo para Datos de Ordenes
  TYPES: BEGIN OF ltp_aufk,
         aufnr TYPE aufnr,        "Número de orden
         auart TYPE aufart,       "Clase de orden
         autyp TYPE auftyp,       "Tipo de orden
         ernam TYPE auferfnam,    "Nombre del autor
         erdat TYPE auferfdat,    "Fecha entrada (Fecha de Creacion de la Orden)
         werks TYPE werks_d,      "Centro
         user0 TYPE aufuser0,     "Solicitante
         kdauf TYPE kdauf,        "Número del pedido de cliente
       END OF ltp_aufk.

*.....Typo para Datos de Ordenes
  TYPES: BEGIN OF ltp_afko,
         aufnr TYPE aufnr,       "Número de orden
         rsnum TYPE rsnum,       "Reserva
         plnbez TYPE matnr,      "Material

       END OF ltp_afko.

*.... Estructuras para buscar las reservas en la tabla resb
*{   REPLACE        ER3K900309                                        2
*\  DATA: lti_resb TYPE STANDARD TABLE OF resb,
*\        les_resb TYPE resb.
  DATA: lti_resb TYPE STANDARD TABLE OF ltp_resb,
        les_resb TYPE ltp_resb.
*}   REPLACE
  DATA :
        detalles_reservas_aux LIKE detalles_reservas,
*..... Tabla interna con datos de la tabla de el aglutinador y la asociación de las ordenes
        lti_zppt_0001 TYPE TABLE OF ltp_zppt_0001,
*..... Estructura con datos de la tabla de el aglutinador y la asociación de las ordenes
        les_zppt_0001 TYPE ltp_zppt_0001,
*.....Tabla Interna con datos Basicos de Orden
        lti_aufk TYPE TABLE OF ltp_aufk,
*.....Tabla Interna con datos Basicos de Orden con material
        lti_afko TYPE TABLE OF ltp_afko,
*.... Estructura con los datos de la cabecera de aglutinador
        les_aglut TYPE zesd_cabecera_aglut,
*.... Aglutinador
        l_e_nmint TYPE zppde_0002.
  DATA:
*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF ze_documento,
          ls_numero LIKE LINE OF rl_numero,
*.....Rango para Clase de Documento
          rl_clase_doc TYPE RANGE OF ze_clas_documento,
          ls_clase_doc LIKE LINE OF rl_clase_doc,
*.....Mensajes de Retorno para la Bapi
          lti_bapiret2 TYPE bapiret2_tab.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.
  FIELD-SYMBOLS: <lfs_actividad> TYPE zmm_t_clase_pv,
                  <lfs_aufk> TYPE ltp_aufk,
                  <lfs_afko> TYPE ltp_afko,
                  <lfs_bapiret2> TYPE bapiret2,
                  <lfs_res_items> TYPE bapi2093_res_items_get,
*                 <lfs_rkpf> TYPE ltp_rkpf,
*                 <lfs_vbak> TYPE ltp_vbak,
                  <lfs_det_res> TYPE bapi2093_res_items_get,
                  <lfs_zppt_001> TYPE ltp_zppt_0001.
*                 <lfs_kna1> TYPE ltp_kna1.

   DATA: les_det_res LIKE LINE OF detalles_reservas,
         lti_mara  TYPE TABLE OF mara,
         les_mara TYPE mara.
*.....Ajusto Número de Documento
  MOVE i_documento_origen TO l_e_nmint.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_nmint
    IMPORTING
      output = l_e_nmint.


*.... Busqueda de aglutinador en la tabla   Agrupador Ordenes de producción
  SELECT * FROM zppt_0001
    INTO CORRESPONDING FIELDS OF TABLE lti_zppt_0001
    WHERE nmint EQ l_e_nmint
    ORDER BY nm_ref_p.


  IF sy-subrc EQ 0.
*.....Busqueda Ordenes  para el algutinador proceso pieza
    SELECT aufnr auart autyp ernam erdat werks user0 kdauf
               FROM aufk
                INTO TABLE lti_aufk
                  FOR ALL ENTRIES IN lti_zppt_0001
                  WHERE aufnr EQ lti_zppt_0001-nm_ref_h.
    IF sy-subrc EQ 0.
*.....Busqueda Ordenes de Ser. para los proceso pieza
      SELECT aufnr rsnum plnbez
                 FROM afko
                  INTO TABLE lti_afko
                    FOR ALL ENTRIES IN lti_aufk
                    WHERE aufnr EQ lti_aufk-aufnr.
      IF sy-subrc EQ 0.
*... Recorrermos  todas las ordenes para traer los materiales reservados
        LOOP AT lti_aufk ASSIGNING <lfs_aufk>.
*.....Consulto Cabecera de Reserva para Setiar Cabecera de Reservas
          SELECT SINGLE rsnum wempf kunnr aufnr
                FROM rkpf
                 INTO cabecera_reservas
                    WHERE aufnr EQ <lfs_aufk>-aufnr.
          IF sy-subrc EQ 0 .
            CLEAR :detalles_reservas_aux.
*.....Llamado a Bapi para Setiar Atributo de Posiciones de Reserva
            CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
              EXPORTING
                reservation_no    = cabecera_reservas-rsnum
                movement          = 'X'
              TABLES
                reservation_items = detalles_reservas_aux
                return            = lti_bapiret2.

            IF detalles_reservas_aux[] IS NOT INITIAL.
*-----------------------------------------------------------------------
*............MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*             Modificado por: Andrés Felipe Castro.
              LOOP AT detalles_reservas_aux INTO les_det_res.
                les_mara-matnr = les_det_res-material.
                APPEND les_mara TO lti_mara.
              ENDLOOP.

*      .....Función para Obtener EANs de un Material
              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                TABLES
                  t_mara = lti_mara.
*...........FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

*.... Consulto las reservas para obtener la unidad de medida base y la cantidad
*{   REPLACE        ER3K900309                                        3
*\              SELECT *
*\                INTO CORRESPONDING FIELDS OF TABLE lti_resb
*\                FROM resb
*\                FOR ALL ENTRIES IN detalles_reservas_aux
*\                WHERE rsnum EQ detalles_reservas_aux-reserv_no AND
*\                rspos EQ detalles_reservas_aux-res_item.
               SELECT  rsnum rspos erfme erfmg meins bdmng bwart
          INTO TABLE lti_resb
                FROM resb
                FOR ALL ENTRIES IN detalles_reservas_aux
                WHERE rsnum EQ detalles_reservas_aux-reserv_no AND
                rspos EQ detalles_reservas_aux-res_item.
*}   REPLACE
*Recorro el detalle obtenido de la Bapi
              LOOP AT detalles_reservas_aux ASSIGNING <lfs_det_res>.

                CLEAR:t_detalle,les_resb.
*... Consulto los EANS de el material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                   EXPORTING
                     matnr            = <lfs_det_res>-material
                    TABLES
*       T_MARA           =
                      t_detalle        = t_detalle
                      t_mensajes       = t_mensajes
                            .
*... Consulto el registro en la reserva
                READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
                                     rspos = <lfs_det_res>-res_item.
*... Convierto la cantidad total de la reserva y la convierto a UMB
                READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_resb-erfme.
                IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
                  IF les_resb IS NOT INITIAL.

                    <lfs_det_res>-entry_qnt = les_resb-erfmg  *  les_detalle-cantidad_ean.
                    <lfs_det_res>-entry_uom = les_resb-meins.
                  ENDIF.
                ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
                  IF les_resb IS NOT INITIAL.
                    <lfs_det_res>-entry_qnt = les_resb-bdmng.
                    <lfs_det_res>-entry_uom = les_resb-meins.
                  ENDIF.
                ENDIF.
*{   INSERT         ER3K900309                                        4
                <LFS_DET_RES>-ENTRY_UOM_ISO = les_resb-ERFME.

*}   INSERT
              ENDLOOP.

            ENDIF.


            APPEND LINES OF detalles_reservas_aux TO detalles_reservas.
          ENDIF.

        ENDLOOP.
*.....Ordeno Tabla
        SORT detalles_reservas BY reserv_no res_item ASCENDING.

      ENDIF.

    ENDIF.

*DELETE adjacent duplicates from lti_zppt_0001   comparing nm_ref_p .
    LOOP AT lti_zppt_0001 INTO les_zppt_0001.
      CLEAR: les_aglut.
*     Asigno aglutinador
      les_aglut-aglutinador = les_zppt_0001-nmint.
      les_aglut-nm_ref_p = les_zppt_0001-nm_ref_p.
*     Consultamos los datos de la orden
      READ TABLE lti_aufk ASSIGNING <lfs_aufk> WITH KEY aufnr = les_zppt_0001-nm_ref_h .
      IF sy-subrc EQ 0.
        les_aglut-nm_ref_h = <lfs_aufk>-aufnr.
        les_aglut-auart = <lfs_aufk>-auart.
        les_aglut-autyp = <lfs_aufk>-autyp.
        les_aglut-ernam = <lfs_aufk>-ernam.
        les_aglut-erdat = <lfs_aufk>-erdat.
        les_aglut-werks = <lfs_aufk>-werks.
        les_aglut-user0 = <lfs_aufk>-user0.
        les_aglut-kdauf = <lfs_aufk>-kdauf.
      ENDIF.
*     Consultamos los datos del material
      READ TABLE lti_afko ASSIGNING <lfs_afko> WITH KEY aufnr = <lfs_aufk>-aufnr.
      IF sy-subrc EQ 0.
*...    Reserva
        les_aglut-rsnum = <lfs_afko>-rsnum.
*...   Asigno material
        les_aglut-matnr = <lfs_afko>-plnbez.
      ENDIF.
*.... Inserto Registro
      APPEND les_aglut TO cabecera_aglutinador.
    ENDLOOP.
*.....Ordeno Tabla de Cabeceras
    SORT cabecera_aglutinador BY rsnum ASCENDING.
  ENDIF.

ENDMETHOD.


METHOD zif_lgtica_documento~set_compras.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo para Setiar Atributos de Cabecera y Detalle de un
*                Documento de Compras
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 07.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 07.07.2014    ER6K906849    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
  DATA:
*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF ebeln,
          ls_numero LIKE LINE OF rl_numero,
          lv_ebeln type ebeln,
*.....Rango para Clase de Documento
          rl_clase_doc TYPE RANGE OF ze_clas_documento,
          ls_clase_doc LIKE LINE OF rl_clase_doc.

*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .



move i_documento_origen to lv_ebeln.

*.....Función para Ajuste de Número
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = lv_ebeln
    importing
      output = lv_ebeln.
*.....Creo Rango de Documento
    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = lv_ebeln.
    APPEND ls_numero TO rl_numero.
  ENDIF.

*.....Rango para Clase de Clase de Documento
  IF i_clase_documento IS NOT INITIAL  .
*.....Creo Rango de Fechas
    ls_clase_doc-sign = 'I'.
    ls_clase_doc-option = 'EQ'.
    ls_clase_doc-low = i_clase_documento .
    APPEND ls_clase_doc TO rl_clase_doc.
  ENDIF.
*... Ajustar documento
    CALL METHOD me->get_ajustar_documento
      EXPORTING
        i_documento = i_documento_origen
      RECEIVING
        r_documento = i_documento_origen.
*.....Consulto Cabecera Compras
*{   REPLACE        ER3K900309                                        1
*\  SELECT *
*\    FROM  ekko
*\       INTO TABLE cabeceras_compras
*\           WHERE ebeln IN rl_numero AND
*\                 bsart IN rl_clase_doc.
  SELECT EBELN BSART  RESWK KUNNR AEDAT LIFNR EKORG
    FROM  ekko
       INTO TABLE cabeceras_compras
           WHERE ebeln IN rl_numero AND
                 bsart IN rl_clase_doc.
*}   REPLACE
  IF sy-subrc EQ 0 .
    SORT cabeceras_compras BY ebeln ASCENDING.

*.....Consulto Detalles de Posiciones de Compras
    SELECT werks ebeln ebelp loekz statu aedat txz01 matnr bukrs lgort matkl idnlf ktmng menge
           meins netpr peinh netwr brtwr mwskz bwtar bwtty abskz elikz pstyp knttp labnr konnr ktpnr
           abdat lmein abmng prdat bstyp kunnr ekkol plifz ntgew gewei revlv geber fistl ko_prctr
           emlif lblkz adrn2 banfn bnfpo mtart uptyp lfret afnam berid status
      FROM ekpo
           INTO TABLE detalles_compras
              FOR ALL ENTRIES IN cabeceras_compras
                  WHERE ebeln EQ cabeceras_compras-ebeln.

*.....Se Ordena la Tabla
    SORT detalles_compras BY werks ebeln ebelp ASCENDING.
  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_documento~set_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo para Setiar Atributo de Entregas para un documento
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Typo para Datos Basicos de Entrega
  TYPES : BEGIN OF ltp_lips,
          vbeln TYPE vbeln_vl, "Entrega
          posnr TYPE posnr_vl, "Posición de entrega
          pstyv TYPE pstyv_vl,
          matnr TYPE matnr,    "Número de material
          werks TYPE werks_d,  "Centro
          lgort TYPE lgort_d,  "Almacén
          charg TYPE charg_d,  "Número de lote
          lfimg TYPE lfimg,    "Cantidad entregada efectivamente en UMV
          meins TYPE meins,    "Unidad de medida base
          vrkme TYPE vrkme,    "Unidad de medida de venta
          vgbel TYPE vgbel,    "Número de documento del documento modelo
          vgpos TYPE vgpos,
          lgnum TYPE lgnum,    "Núm.almacén/Complejo alm.
          sernr TYPE sernr,    "Número de serie
          ormng TYPE ormng_vl,
          customer TYPE ekunn, "Número de cuenta del cliente
          orderid TYPE aufnr,  "Número de orden
          order_itno TYPE co_posnr, "Número de posición de orden
        END OF ltp_lips.

  DATA:
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
*.... Estructura para llenar la información de retorno de la entrega
          les_entrega TYPE zesd_det_ent_pkg,
*.... Variable para almacenar el número de documento
          v_vgbel TYPE vgbel,
*.....Estructura para Cabecera Ventas
*{   REPLACE        ER3K900309                                        1
*\          les_cab_ventas TYPE vbak,
          les_cab_ventas TYPE ZESD_CAB_PED,
*}   REPLACE
*{   REPLACE        ER3K900309                                        2
*\          les_cab_compras TYPE EKKO,
          les_cab_compras TYPE ZESD_CAB_CMP,
*}   REPLACE

*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF vgbel,
          ls_numero LIKE LINE OF rl_numero.
*.... Limpio el atributo de entregas.
CLEAR: zif_lgtica_documento~t_entregas.
*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .
    MOVE i_documento_origen TO v_vgbel.
*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_vgbel
      IMPORTING
        output = v_vgbel.
*.....Creo Rango de Documento
    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = v_vgbel.
    APPEND ls_numero TO rl_numero.
  ENDIF.

*.... Se toman las entregas asociadas al Número de documento ingresado
  SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
  INTO TABLE lti_lips
    FROM lips
    WHERE vgbel IN rl_numero.

  IF sy-subrc EQ 0.
    READ TABLE cabeceras_ventas INDEX 1 INTO les_cab_ventas.
    IF  sy-subrc EQ 0 .
*.... Recorro las entregas encontradas y las ingreso en el atributo de T_ENTREGAS
      LOOP AT  lti_lips INTO les_lips.
        les_entrega-documento = les_lips-vgbel. "Número de un Documento.
        les_entrega-entrega = les_lips-vbeln.   "Entrega
        les_entrega-posnr = les_lips-posnr.
        les_entrega-pstyv = les_lips-pstyv.
        les_entrega-matnr = les_lips-matnr.
        les_entrega-werks = les_lips-werks.
        les_entrega-lgort = les_lips-lgort.
        les_entrega-charg = les_lips-charg.
        les_entrega-lfimg = les_lips-lfimg.
        les_entrega-meins = les_lips-meins.
        les_entrega-vrkme = les_lips-vrkme.
        les_entrega-vgbel = les_lips-vgbel.
        les_entrega-vgpos = les_lips-vgpos.
        les_entrega-lgnum = les_lips-lgnum.     "Núm.almacén/Complejo alm.
        les_entrega-sernr = les_lips-sernr.     "Número de serie
        les_entrega-ormng = les_lips-ormng.
        les_entrega-customer = les_cab_ventas-kunnr.
        les_entrega-orderid = les_cab_ventas-aufnr.
        les_entrega-order_itno = les_lips-posnr.
        APPEND les_entrega TO zif_lgtica_documento~t_entregas.
        CLEAR : les_entrega.
      ENDLOOP.
*.....Ordeno Tabla
      SORT zif_lgtica_documento~t_entregas BY documento entrega ASCENDING.
    ELSE.
      READ TABLE cabeceras_compras INDEX 1 INTO les_cab_compras.
      IF  sy-subrc EQ 0 .
*.... Recorro las entregas encontradas y las ingreso en el atributo de T_ENTREGAS
      LOOP AT  lti_lips INTO les_lips.
        les_entrega-documento = les_lips-vgbel. "Número de un Documento.
        les_entrega-entrega = les_lips-vbeln.   "Entrega
        les_entrega-posnr = les_lips-posnr.
        les_entrega-pstyv = les_lips-pstyv.
        les_entrega-matnr = les_lips-matnr.
        les_entrega-werks = les_lips-werks.
        les_entrega-lgort = les_lips-lgort.
        les_entrega-charg = les_lips-charg.
        les_entrega-lfimg = les_lips-lfimg.
        les_entrega-meins = les_lips-meins.
        les_entrega-vrkme = les_lips-vrkme.
        les_entrega-vgbel = les_lips-vgbel.
        les_entrega-vgpos = les_lips-vgpos.
        les_entrega-lgnum = les_lips-lgnum.     "Núm.almacén/Complejo alm.
        les_entrega-sernr = les_lips-sernr.     "Número de serie
        les_entrega-ormng = les_lips-ormng.
        les_entrega-customer = les_cab_compras-kunnr.
*        les_entrega-orderid = les_cab_compras-aufnr.
        les_entrega-order_itno = les_lips-posnr.
        APPEND les_entrega TO zif_lgtica_documento~t_entregas.
        CLEAR : les_entrega.
      ENDLOOP.
*.....Ordeno Tabla
      SORT zif_lgtica_documento~t_entregas BY documento entrega ASCENDING.
    ENDIF.
    ENDIF.
    CLEAR : les_cab_ventas.
  ENDIF.

ENDMETHOD.


METHOD zif_lgtica_documento~set_ordenes.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo para Setiar Atributos de Cabecera y Detalle de Ordenes
*                de Servicio Internas, Reservas de Inventario, Ordenes de
*                Produccion sin Aglutinador
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 13.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 13.08.2014    ER6K907163    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
*{   INSERT         ER3K900309                                        1
TYPES: BEGIN OF ltp_resb,
 rsnum    type resb-rsnum,
 rspos    type resb-rspos,
 erfme    type resb-erfme,
 erfmg    type resb-erfmg,
 meins    type resb-meins,
 bdmng    type resb-bdmng,
 bwart    type resb-bwart,
  END OF ltp_resb.
*}   INSERT
*.... Estructuras para buscar las reservas en la tabla resb
*{   REPLACE        ER3K900309                                        2
*\  DATA: lti_resb TYPE STANDARD TABLE OF resb,
*\        les_resb TYPE resb.
  DATA: lti_resb TYPE STANDARD TABLE OF ltp_resb,
        les_resb TYPE ltp_resb.
*}   REPLACE
  DATA:
*..... Index de la reserva
        l_e_index type sy-tabix,
*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF ze_documento,
          ls_numero LIKE LINE OF rl_numero,
*.....Rango para Clase de Documento
          rl_clase_doc TYPE RANGE OF ze_clas_documento,
          ls_clase_doc LIKE LINE OF rl_clase_doc,
*.....Mensajes de Retorno para la Bapi
          lti_bapiret2 TYPE bapiret2_tab.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_picking.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.

  DATA: les_det_res LIKE LINE OF detalles_reservas,
        lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_det_res> TYPE bapi2093_res_items_get.
*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .

    CALL METHOD me->get_ajustar_documento
      EXPORTING
        i_documento = i_documento_origen
      RECEIVING
        r_documento = i_documento_origen.

*.....Creo Rango de Documento
    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = i_documento_origen .
    APPEND ls_numero TO rl_numero.
  ENDIF.

*.....Rango para Clase de Clase de Documento
  IF i_clase_documento IS NOT INITIAL  .
*.....Creo Rango de Fechas
    ls_clase_doc-sign = 'I'.
    ls_clase_doc-option = 'EQ'.
    ls_clase_doc-low = i_clase_documento .
    APPEND ls_clase_doc TO rl_clase_doc.
  ENDIF.

*{   REPLACE        ER3K900309                                       11
*\*.....Consulto Cabecera de Ordenes de Servicio y Producción para Setiar Cabecera de Ordenes
*\  SELECT SINGLE aufnr auart autyp ernam erdat werks user0 kdauf
*\     FROM aufk
*\        INTO cabeceras_ordenes
*\          WHERE aufnr IN rl_numero AND
*\                auart IN rl_clase_doc.
*.....Consulto Cabecera de Ordenes de Servicio y Producción para Setiar Cabecera de Ordenes
  SELECT SINGLE aufnr auart autyp ernam erdat werks user0 kdauf
     FROM aufk
        INTO cabeceras_ordenes
          WHERE aufnr IN rl_numero.

*}   REPLACE

  IF sy-subrc EQ 0.
*.....Consulto Cabecera de Reserva para Setiar Cabecera de Reservas
    SELECT SINGLE rsnum wempf kunnr aufnr
          FROM rkpf
           INTO cabecera_reservas
              WHERE aufnr EQ cabeceras_ordenes-aufnr.
    IF sy-subrc EQ 0 .
*.....Llamado a Bapi para Setiar Atributo de Posiciones de Reserva
      CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
        EXPORTING
          reservation_no    = cabecera_reservas-rsnum
          movement          = 'X'
        TABLES
          reservation_items = detalles_reservas
          return            = lti_bapiret2.
*.....Ordeno Tabla
      SORT detalles_reservas BY reserv_no res_item  ASCENDING.
*{   INSERT         ER3K900309                                        9
      DELETE detalles_reservas where plant ne detalles_usuario-centro OR stge_loc ne detalles_usuario-almacen.
*}   INSERT

*****
      IF detalles_reservas[] IS NOT INITIAL.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
        LOOP AT detalles_reservas INTO les_det_res.
          les_mara-matnr = les_det_res-material.
          APPEND les_mara TO lti_mara.
        ENDLOOP.

*.....Función para Obtener EANs de un Material
        CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
          TABLES
            t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*.... Consulto las reservas para obtener la unidad de medida base y la cantidad
*{   REPLACE        ER3K900309                                        3
*\        SELECT *
*\          INTO CORRESPONDING FIELDS OF TABLE lti_resb
*\          FROM resb
*\          FOR ALL ENTRIES IN detalles_reservas
*\          WHERE rsnum EQ detalles_reservas-reserv_no AND
*\          rspos EQ detalles_reservas-res_item AND bwart NE '262'.
        SELECT  rsnum rspos erfme erfmg meins bdmng bwart
          INTO TABLE lti_resb
          FROM resb
          FOR ALL ENTRIES IN detalles_reservas
          WHERE rsnum EQ detalles_reservas-reserv_no AND
          rspos EQ detalles_reservas-res_item .

          DELETE lti_resb where bwart eq '262'.
*}   REPLACE
*Recorro el detalle obtenido de la Bapi
*{   REPLACE        ER3K900309                                        7
*\        LOOP AT detalles_reservas ASSIGNING <lfs_det_res>.
        LOOP AT detalles_reservas ASSIGNING <lfs_det_res> where plant = detalles_usuario-centro and stge_loc = detalles_usuario-almacen.
*}   REPLACE
            l_e_index = sy-tabix.
**          READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
**          rspos = <lfs_det_res>-res_item.
***... Elimino el registro si no lo encuentra
**          IF sy-subrc NE 0.
**            DELETE detalles_reservas FROM <lfs_det_res>.
**          ELSEIF  sy-subrc EQ 0.
***....Asigno la unidad de medida base y la cantidad si la encuentra
**            <lfs_det_res>-entry_qnt = les_resb-bdmng.
**            <lfs_det_res>-entry_uom = les_resb-meins.
**          ENDIF.
*... Consulto el registro en la reserva
          READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
                                   rspos = <lfs_det_res>-res_item.
          IF sy-subrc NE 0.
            DELETE detalles_reservas index l_e_index.
            CONTINUE.
          ENDIF.

*{   REPLACE        ER3K900309                                        5
*\          CLEAR:t_detalle,les_resb.
          CLEAR:t_detalle.
*}   REPLACE
*... Consulto los EANS de el material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
             EXPORTING
               matnr            = <lfs_det_res>-material
              TABLES
*       T_MARA           =
                t_detalle        = t_detalle
                t_mensajes       = t_mensajes

                      .
**... Consulto el registro en la reserva
*          READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
*                               rspos = <lfs_det_res>-res_item.
*... Convierto la cantidad total de la reserva y la convierto a UMB
          READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_resb-erfme.
          IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
            IF les_resb IS NOT INITIAL.

              <lfs_det_res>-entry_qnt = les_resb-erfmg  *  les_detalle-cantidad_ean.
              <lfs_det_res>-entry_uom = les_resb-meins.
            ENDIF.
          ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
            IF les_resb IS NOT INITIAL.
              <lfs_det_res>-entry_qnt = les_resb-bdmng.
              <lfs_det_res>-entry_uom = les_resb-meins.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.
*.....No Existe la Orden en Base de Datos o es una Reserva de Inventario
  ELSE.
*.....Consulto Cabecera de Reserva para Setiar Cabecera de Reservas
    SELECT SINGLE rsnum wempf kunnr aufnr
          FROM rkpf
           INTO cabecera_reservas
              WHERE rsnum EQ i_documento_origen.
    IF sy-subrc EQ 0 .
*.....Llamado a Bapi para Setiar Atributo de Posiciones de Reserva
      CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
        EXPORTING
          reservation_no    = cabecera_reservas-rsnum
          movement          = 'X'
        TABLES
          reservation_items = detalles_reservas
          return            = lti_bapiret2.
*.....Ordeno Tabla
      SORT detalles_reservas BY reserv_no res_item ASCENDING.
*{   INSERT         ER3K900309                                       10
      DELETE detalles_reservas where plant ne detalles_usuario-centro OR stge_loc ne detalles_usuario-almacen.
*}   INSERT

      IF detalles_reservas[] IS NOT INITIAL.
*.... Consulto las reservas para obtener la unidad de medida base y la cantidad
*{   REPLACE        ER3K900309                                        4
*\        SELECT *
*\          INTO CORRESPONDING FIELDS OF TABLE lti_resb
*\          FROM resb
*\          FOR ALL ENTRIES IN detalles_reservas
*\          WHERE rsnum EQ detalles_reservas-reserv_no AND
*\          rspos EQ detalles_reservas-res_item AND bwart NE '262'.
        SELECT  rsnum rspos erfme erfmg meins bdmng bwart
          INTO TABLE lti_resb
          FROM resb
          FOR ALL ENTRIES IN detalles_reservas
          WHERE rsnum EQ detalles_reservas-reserv_no AND
          rspos EQ detalles_reservas-res_item .

          DELETE lti_resb where bwart eq '262'.
*}   REPLACE
*Recorro el detalle obtenido de la Bapi
*{   REPLACE        ER3K900309                                        8
*\        LOOP AT detalles_reservas ASSIGNING <lfs_det_res>.
        LOOP AT detalles_reservas ASSIGNING <lfs_det_res> where plant = detalles_usuario-centro and stge_loc = detalles_usuario-almacen.
*}   REPLACE
          l_e_index = sy-tabix.
*... Consulto el registro en la reserva
          READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
                                   rspos = <lfs_det_res>-res_item.
          IF sy-subrc NE 0.
            DELETE detalles_reservas index l_e_index.
            CONTINUE.
          ENDIF.

*{   REPLACE        ER3K900309                                        6
*\          CLEAR:t_detalle,les_resb.
          CLEAR:t_detalle.
*}   REPLACE
*... Consulto los EANS de el material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
             EXPORTING
               matnr            = <lfs_det_res>-material
              TABLES
*       T_MARA           =
                t_detalle        = t_detalle
                t_mensajes       = t_mensajes.
*                      .
**... Consulto el registro en la reserva
*          READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_det_res>-reserv_no
*                               rspos = <lfs_det_res>-res_item.
*... Convierto la cantidad total de la reserva y la convierto a UMB
          READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_resb-erfme.
          IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
            IF les_resb IS NOT INITIAL.

              <lfs_det_res>-entry_qnt = les_resb-erfmg  *  les_detalle-cantidad_ean.
              <lfs_det_res>-entry_uom = les_resb-meins.
            ENDIF.
          ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
            IF les_resb IS NOT INITIAL.
              <lfs_det_res>-entry_qnt = les_resb-bdmng.
              <lfs_det_res>-entry_uom = les_resb-meins.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD zif_lgtica_documento~set_ventas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo para Setiar Atributos de Cabecera y Detalle de un
*                Documento de Ventas
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 07.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 07.07.2014    ER6K906849    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

  DATA:
*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF vbeln,
          ls_numero LIKE LINE OF rl_numero,
          lv_vbeln TYPE vbeln,
*.....Rango para Clase de Documento
          rl_clase_doc TYPE RANGE OF ze_clas_documento,
          ls_clase_doc LIKE LINE OF rl_clase_doc.

*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .



    MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_vbeln
      IMPORTING
        output = lv_vbeln.

*.....Creo Rango de Documento
    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = lv_vbeln .
    APPEND ls_numero TO rl_numero.
  ENDIF.

*.....Rango para Clase de Clase de Documento
  IF i_clase_documento IS NOT INITIAL  .
*.....Creo Rango de Fechas
    ls_clase_doc-sign = 'I'.
    ls_clase_doc-option = 'EQ'.
    ls_clase_doc-low = i_clase_documento .
    APPEND ls_clase_doc TO rl_clase_doc.
  ENDIF.

*.....Consulto Cabecera Ventas
*{   REPLACE        ER3K900309                                        1
*\  SELECT *
*\     FROM  vbak
*\        INTO TABLE cabeceras_ventas
*\          WHERE vbeln IN rl_numero AND
*\                auart IN rl_clase_doc.
  SELECT VBELN  KUNNR AUFNR
     FROM  vbak
        INTO TABLE cabeceras_ventas
          WHERE vbeln IN rl_numero AND
                auart IN rl_clase_doc.
*}   REPLACE

  IF sy-subrc EQ 0 .
    SORT cabeceras_ventas BY vbeln ASCENDING.

*.....Consulto Detalles de Posiciones Ventas
    SELECT werks vbeln posnr matnr charg matkl arktx pstyv posar lfrel zmeng
           zieme posex kdmat vkaus grkor fmeng spart gsber netwr waerk antlf kztlf
           kwmeng lsmeng kbmeng klmeng vrkme umvkz umvkn vbelv posnv vgbel vgpos upflu lprio
           lgort vstel route stkey stdat stlnr erdat ernam erzet vgref netpr kpein kmein shkzg
           bwtar aedat fixmg prctr sobkz vpzuo aufnr vpmat vpwrk prbme sernr objnr kostl
       FROM  vbap
         INTO TABLE detalles_ventas
         FOR ALL ENTRIES IN cabeceras_ventas
            WHERE vbeln EQ cabeceras_ventas-vbeln.

*.....Ordeno Tabla
    SORT detalles_ventas BY werks vbeln posnr ASCENDING.

  ENDIF.
ENDMETHOD.


method ZIF_LGTICA_PACKING~GET_CABECERA_PACKING.
endmethod.


  method ZIF_LGTICA_PACKING~GET_DETALLE_PACKING.
  endmethod.


method ZIF_LGTICA_PACKING~GET_HEADER_PACKING.
endmethod.


  method ZIF_LGTICA_PACKING~GET_POS_PACKING.
  endmethod.


  method ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING.
  endmethod.


method ZIF_LGTICA_PICKABLE~GET_POS_PICKING.
endmethod.
ENDCLASS.
