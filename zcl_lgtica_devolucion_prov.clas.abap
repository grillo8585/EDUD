class ZCL_LGTICA_DEVOLUCION_PROV definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_DEVOLUCION_PROV
*"* do not include other source files here!!!
public section.

  methods ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_UBICACION_SUGERIDA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_POS_PICKING
    redefinition .
protected section.
*"* protected components of class ZCL_LGTICA_DEVOLUCION_PROV
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_DEVOLUCION_PROV
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_DEVOLUCION_PROV IMPLEMENTATION.


METHOD zif_lgtica_documento~generar_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Entregas para Proceso Devolucion Proveedores
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 04.07.2014    ER6K906899    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Typo para Procesamiento de Errores al Generar la Entrega
*{   REPLACE        ER3K900309                                        1
*\  TYPES : BEGIN OF ltp_vbfs,
*\          sammg TYPE sammg,      "Grupo
*\          msgid TYPE msgid,      "Clase de Mensaje
*\          msgno TYPE msgno,      "Número del mensaje de sistema
*\          msgty TYPE msgty,      "Tipo de mensaje
*\        END OF ltp_vbfs.
  TYPES : BEGIN OF ltp_vbfs,
          sammg TYPE sammg,      "Grupo
          msgid TYPE msgid,      "Clase de Mensaje
          msgno TYPE msgno,      "Número del mensaje de sistema
          msgty TYPE msgty,      "Tipo de mensaje
          MSGV1	type MSGV1,      "Párametro 1
          MSGV2	type MSGV2,      "Párametro 2
          MSGV3	type MSGV3,      "Párametro 3
          MSGV4	type MSGV4,      "Párametro 4
        END OF ltp_vbfs.
*}   REPLACE

  DATA:
*.....Tabla Con msg de Errores
        lti_vbfs TYPE TABLE OF ltp_vbfs,
        les_vbfs TYPE ltp_vbfs,
        les_bapiret1 TYPE bapiret1.

*.....Typo para Completar Datos Necesarios para la Generación de Entrega
  TYPES : BEGIN OF ltp_ekpv,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
          vstel TYPE vstel,
          lprio TYPE lprio,
          route TYPE route,
          kunnr TYPE kunnr,
          vkorg TYPE vkorg,
          vtweg TYPE vtweg,
          spart TYPE spart,
          ledat TYPE ledat,
          END OF ltp_ekpv.
  DATA:
*.....Tabla Interna con Datos Necesarios para Generación de Entrega
        lti_key_enque_read TYPE shp_vl10_package_t,
        lti_vetvg TYPE shp_vl10_vetvg_t,
*.....Estructura para Generación de Entrega
        les_key_enque_read TYPE shp_vl10_package,
        les_vetvg TYPE vetvg,
*.....Tabla Interna con Datos Adicionales de la Compra
        lti_ekpv TYPE TABLE OF ltp_ekpv,
*.....Tabla Interna con Entregas
        lti_vbls TYPE shp_vbls_t,
*.....Tabla Interna con Proceso de Errores
       lti_vbsk TYPE shp_vbsk_t,
*.....Variable con Entrega
        les_entregas TYPE zesd_detalles_entregas,
*.....Estructuras para Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Mensaje de Respuesta
        l_e_msg TYPE string,
*.....Variable Fecha
        l_e_fecha TYPE sy-datum,
*{   INSERT         ER3K900309                                        5
      e_syst type syst,
      number type SYMSGNO,
*}   INSERT

        lo_pick TYPE REF TO zif_lgtica_pickable,
        lo_pick_clas TYPE REF TO zcl_lgtica_picking.

*.....Field Simbols para Procesamiento de Información de Compras
  FIELD-SYMBOLS: <lfs_ekpo> TYPE zesd_posiciones_compras,
*{   REPLACE        ER3K900309                                       11
*\                 <lfs_ekko> TYPE ekko,
                 <lfs_ekko> TYPE ZESD_CAB_CMP,
*}   REPLACE
                 <lfs_ekpv> TYPE ltp_ekpv,
                 <lfs_vbls> TYPE vbls.

*.....Metodo para Setiar Atributos
  CALL METHOD me->zif_lgtica_documento~set_compras
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Verifico que no Existan Errores en la Información Correspondiente al Usuario
  IF detalles_usuario IS NOT INITIAL  .
    SELECT ebeln ebelp vstel lprio route kunnr vkorg vtweg spart ledat
       FROM ekpv
         INTO TABLE lti_ekpv
           FOR ALL ENTRIES IN detalles_compras
            WHERE ebeln EQ detalles_compras-ebeln.

    IF sy-subrc EQ 0 .
*.....Verifico el Número de Entregas a Generar
*{   INSERT         ER3K900309                                       10

      DELETE ADJACENT DUPLICATES FROM detalles_compras COMPARING werks.
      DELETE detalles_compras  where werks NE detalles_usuario-centro.

*}   INSERT
      LOOP AT detalles_compras  ASSIGNING <lfs_ekpo>
                                WHERE werks EQ detalles_usuario-centro.
*.....Para Centro Generar la Entrega
        AT END OF werks.
          CLEAR :les_key_enque_read, les_vetvg, lti_key_enque_read, lti_vetvg, les_entregas,
                 lti_vbls.

*.....Busco la Fecha de Creacíon del Documento
          READ TABLE cabeceras_compras WITH KEY ebeln = <lfs_ekpo>-ebeln
                                        ASSIGNING <lfs_ekko>.
          READ TABLE lti_ekpv INDEX 1 ASSIGNING <lfs_ekpv>.

          IF sy-subrc EQ 0 .
            l_e_fecha = sy-datum.
            les_key_enque_read-panum = '1'.
            les_key_enque_read-vbobj = 'B'.
            les_key_enque_read-vbtyp = 'V'.
            les_key_enque_read-vbeln = <lfs_ekko>-ebeln.
            les_key_enque_read-id = '1'.
            les_key_enque_read-kzazu = 'X'.
            les_key_enque_read-ledat = l_e_fecha. "<lfs_ekko>-aedat.
            les_key_enque_read-tabix = '1'.
            READ TABLE lti_ekpv WITH KEY ebeln = <lfs_ekko>-ebeln ASSIGNING <lfs_ekpv>.
            IF sy-subrc EQ 0 .
              les_key_enque_read-vstel = <lfs_ekpv>-vstel.
              les_key_enque_read-kunwe = <lfs_ekpv>-kunnr.
            ENDIF.
            APPEND les_key_enque_read TO lti_key_enque_read.

            les_vetvg-vstel = <lfs_ekpv>-vstel.
            les_vetvg-ledat = <lfs_ekpv>-ledat.
            les_vetvg-lprio = <lfs_ekpv>-lprio.
            les_vetvg-route = <lfs_ekpv>-route.
            les_vetvg-kunwe = <lfs_ekpv>-kunnr.
            les_vetvg-vbeln = <lfs_ekko>-ebeln.
            les_vetvg-vkorg = <lfs_ekpv>-vkorg.
            les_vetvg-vtweg = <lfs_ekpv>-vtweg.
            les_vetvg-spart = <lfs_ekpv>-spart.
            les_vetvg-auart = <lfs_ekko>-bsart.
            les_vetvg-reswk = <lfs_ekko>-reswk.
            APPEND les_vetvg TO lti_vetvg.

            CALL METHOD me->call_bapi_delivery1
              EXPORTING
                i_ledat           = l_e_fecha "<lfs_ekko>-aedat
                i_nur_vorgabe_pos = ' '
                i_key_enque_read  = lti_key_enque_read
                i_vetvg           = lti_vetvg
              IMPORTING
                e_vbls            = lti_vbls
*{   REPLACE        ER3K900309                                        6
*\                e_vbsk            = lti_vbsk.
                e_vbsk            = lti_vbsk
                e_syst            = e_syst.
*}   REPLACE

            IF  lti_vbls[] IS NOT INITIAL .
*.....Ordeno Tabla
              SORT  lti_vbls BY  vbeln_lif posnr_lif ASCENDING.
*.....Elimino Registros Duplicados
*            DELETE ADJACENT DUPLICATES FROM lti_vbls COMPARING vbeln_lif posnr_lif.
**.....Actualizo Base de Datos
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.

              LOOP AT lti_vbls ASSIGNING <lfs_vbls> .
                les_entregas-documento = <lfs_ekko>-ebeln.
                les_entregas-entrega =  <lfs_vbls>-vbeln_lif.
                APPEND les_entregas TO c_entregas.
                CLEAR : les_entregas.
              ENDLOOP.
            ELSE.
*.....Proceso de Manejo de Errores
              IF lti_vbsk IS NOT INITIAL .
*{   REPLACE        ER3K900309                                        2
*\                SELECT sammg msgid msgno msgty
                SELECT sammg msgid msgno msgty MSGV1 MSGV2 MSGV3 MSGV4
*}   REPLACE
                     FROM vbfs
                       INTO TABLE lti_vbfs
                            FOR ALL ENTRIES IN lti_vbsk
                              WHERE sammg EQ lti_vbsk-sammg AND
                                    smart EQ lti_vbsk-smart.
                IF sy-subrc EQ 0 .
                  LOOP AT lti_vbfs INTO les_vbfs.
*{   REPLACE        ER3K900309                                        3
*\                    CALL FUNCTION 'BALW_BAPIRETURN_GET1'
*\                      EXPORTING
*\                        type       = les_vbfs-msgty
*\                        cl         = les_vbfs-msgid
*\                        number     = les_vbfs-msgno
*\                      IMPORTING
*\                        bapireturn = les_bapiret1.
                               number = les_vbfs-msgno.
                    CALL FUNCTION 'BALW_BAPIRETURN_GET1'
                      EXPORTING
                        type       = les_vbfs-msgty
                        cl         = les_vbfs-msgid
                        number     = number
                        PAR1             = les_vbfs-MSGV1
                        PAR2             = les_vbfs-MSGV2
                        PAR3             = les_vbfs-MSGV3
                        PAR4             = les_vbfs-MSGV4

                      IMPORTING
                        bapireturn = les_bapiret1.
*}   REPLACE

                    CONCATENATE 'Entrega para el Doc ' space <lfs_ekko>-ebeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_ekko>-ebeln.
*{   REPLACE        ER3K900309                                        8
*\                    les_msg-msg = l_e_msg.
                    les_msg-msg = 'E'.
*}   REPLACE
                    les_msg-type_msg = les_bapiret1-type.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
                  ENDLOOP.
                  CLEAR : lti_vbfs.
*{   INSERT         ER3K900309                                        7
                ELSE.
                  number = e_syst-msgno.
                  CALL FUNCTION 'BALW_BAPIRETURN_GET1'
                      EXPORTING
                        type       = e_syst-msgty
                        cl         = e_syst-msgid
                        number     = number
                        PAR1             = e_syst-MSGV1
                        PAR2             = e_syst-MSGV2
                        PAR3             = e_syst-MSGV3
                        PAR4             = e_syst-MSGV4

                      IMPORTING
                        bapireturn = les_bapiret1.

                      CONCATENATE 'Entrega para el Doc ' space <lfs_ekko>-ebeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_ekko>-ebeln.
                    les_msg-msg = l_e_msg.
                    les_msg-msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
*}   INSERT
                ENDIF.
              ELSE.
*{   REPLACE        ER3K900309                                        4
*\                CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln space
*\                          ' Centro Nro' space <lfs_ekpo>-werks  space  'Verifique Información'
*\                           INTO l_e_msg RESPECTING BLANKS.
*\                les_msg-num_doc = <lfs_ekko>-ebeln.
*\                les_msg-msg = l_e_msg.
*\                les_msg-type_msg = 'E'.
*\                APPEND les_msg TO c_mensajes.
                   number = e_syst-msgno.
                    CALL FUNCTION 'BALW_BAPIRETURN_GET1'
                      EXPORTING
                        type       = e_syst-msgty
                        cl         = e_syst-msgid
                        number     = number
                        PAR1             = e_syst-MSGV1
                        PAR2             = e_syst-MSGV2
                        PAR3             = e_syst-MSGV3
                        PAR4             = e_syst-MSGV4

                      IMPORTING
                        bapireturn = les_bapiret1.

                      CONCATENATE 'Entrega para el Doc ' space <lfs_ekko>-ebeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_ekko>-ebeln.
                    les_msg-msg = l_e_msg.
                    les_msg-msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.

*                CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln space
*                          ' Centro Nro' space <lfs_ekpo>-werks  space  'Verifique Información'
*                           INTO l_e_msg RESPECTING BLANKS.
*                les_msg-num_doc = <lfs_ekko>-ebeln.
*                les_msg-msg = l_e_msg.
*                les_msg-type_msg = 'E'.
*                APPEND les_msg TO c_mensajes.
*}   REPLACE
              ENDIF.
            ENDIF.
          ENDIF.
        ENDAT.
      ENDLOOP.
*.....Elimino Registros Duplicados
      DELETE ADJACENT DUPLICATES FROM c_entregas COMPARING documento entrega.
*       me->entregas = c_entregas.
      me->zif_lgtica_documento~t_entregas = c_entregas.
      lo_pick = me.
*....
*{   REPLACE        ER3K900309                                        9
*\      CALL METHOD zcl_lgtica_picking=>create_by_pkble_ref
      CALL METHOD zcl_lgtica_picking=>CREATE_BY_PKBLE_REF_PCMP
*}   REPLACE
        EXPORTING
          i_ref_pickable = lo_pick.
*.....Actualizo Base de Datos
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CONCATENATE 'Documento' space i_documento_origen space 'No es Valido' space
      'Verifique Información ' space INTO l_e_msg RESPECTING BLANKS.
      les_msg-num_doc =  i_documento_origen.
      les_msg-msg = l_e_msg.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.
  ELSE.
    c_mensajes[] = mensajes[].
    RETURN.
  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_documento~get_cabecera.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Subcontratados
* Autor Prog.  :
* Fecha Creac. : 20.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------*
* Definición de Constantes
*-------------------------------------------------------------------------------*
*CONSTANT: ………………….
*……………..
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_cod_mod TYPE c LENGTH 4 VALUE 'DEVO'.
*CONSTANTS : lc_proceso type c
*-------------------------------------------------------------------------------*
* Definición de Tipos Locales
*-------------------------------------------------------------------------------*
*TYPES: ….
*…………..
*... tipo para la cabecera del documento de compras
  TYPES:BEGIN OF ltp_ti_ekko,
          ebeln TYPE ekko-ebeln, " Número del documento de compras
          bukrs TYPE ekko-bukrs, " Sociedad
          bsart TYPE ekko-bsart, " Clase de documento de compras
          lifnr TYPE ekko-lifnr, " Número de cuenta del proveedor
          reswk TYPE ekko-reswk, " Centro suministrador en el pedido de transporte
          aedat TYPE ekko-aedat, " fecha de creación

         END OF ltp_ti_ekko.
*... tipo para la posición del documento de compras
  TYPES: BEGIN OF ltp_ti_ekpo,
*{   REPLACE        ER3K900279                                        4
*\          ebeln TYPE ekpo-ebeln, " Número del documento de compras
*\          ebelp TYPE ekpo-ebelp, " Número de posición del documento de compras
*\          matnr TYPE ekpo-matnr, " Número de material
*\          bukrs TYPE ekpo-bukrs, " Sociedad
*\          werks TYPE ekpo-werks, " Centro
*\          lgort TYPE ekpo-lgort, " Almacen
*\          menge TYPE ekpo-menge, " Cantidad de pedido
*\          meins TYPE ekpo-meins, " Unidad de medida de pedido
          ebeln TYPE 	ebeln,   "ekpo
          ebelp	 TYPE ebelp,   "ekpo
          matnr TYPE matnr,    "ekpo
          txz01 TYPE maktx,    "ekpo
          menge TYPE ze_por_picking,   "ekpo
          meins TYPE  ze_uni_med,      "ekpo
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          bsart TYPE esart,            "EKKO Clase de documento de compras
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          seriales TYPE	c LENGTH 18,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE  charg_d,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
*}   REPLACE
         END OF ltp_ti_ekpo.

*... tipo para la posición del documento de compras
  TYPES: BEGIN OF ltp_ti_eket,
          ebeln TYPE eket-ebeln, " Número del documento de compras
          ebelp TYPE eket-ebelp, " Número de posición del documento de compras
          menge TYPE eket-menge, " Cantidad de reparto
          wamng TYPE eket-wamng, " Cantidad de salida
         END OF ltp_ti_eket.

*.....Typo para datos del Proveedor
  TYPES: BEGIN OF ltp_lfa1,
        lifnr TYPE lifnr,
        name1 TYPE name1_gp,
        END OF ltp_lfa1.

*{   INSERT         ER3K900279                                        1
*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_eket,
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
   END OF ltp_eket.

*.....Typo para Busqueda Detalle Cabecera Documento EKKO
  TYPES: BEGIN OF ltp_bus_cab,
          ebeln TYPE ekko-ebeln, " Número del documento de compras
          bukrs TYPE ekko-bukrs, " Sociedad
          bsart TYPE ekko-bsart, " Clase de documento de compras
          lifnr TYPE ekko-lifnr, " Número de cuenta del proveedor
          reswk TYPE ekko-reswk, " Centro suministrador en el pedido de transporte
          aedat TYPE ekko-aedat, " fecha de creación
  END OF ltp_bus_cab.

*.....Typo para Busqueda Detalle  Posiciones del Documento EKPO
  TYPES : BEGIN OF ltp_bus_pos,
          ebeln TYPE 	ebeln,   "ekpo
          ebelp	 TYPE ebelp,   "ekpo
          matnr TYPE matnr,    "ekpo
          txz01 TYPE maktx,    "ekpo
          menge TYPE ze_por_picking,   "ekpo
          meins TYPE  ze_uni_med,      "ekpo
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          bsart TYPE esart,            "EKKO Clase de documento de compras
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          seriales TYPE	c LENGTH 18,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE  charg_d,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
*          ean TYPE ean11,              "ZESD_DET_REF Número de artículo europeo (EAN)
*          cantidad_ean  TYPE zed_cantidad_ean, "ZESD_DET_REF Cantidad EAN decimal
*          unidad_med TYPE c LENGTH 3,  "Unidad de medida
    END OF ltp_bus_pos.

*}   INSERT


*-------------------------------------------------------------------------------*
* Definición de Estructuras Tables
*-------------------------------------------------------------------------------*
*TABLES: t001.   "Códigos de Compañía

*-------------------------------------------------------------------------------*
* Definición de Estructuras
*-------------------------------------------------------------------------------*
  DATA:
*.....Tabla Interna con datos de Proveedor
      lti_lfa1 TYPE TABLE OF ltp_lfa1,
*.....Estructura para datos de Proveedor
      les_lfa1 TYPE ltp_lfa1.


*DATA:
  DATA: les_ekko TYPE ltp_ti_ekko.
  DATA: les_ekpo TYPE ltp_ti_ekpo.
  DATA: les_eket TYPE ltp_ti_eket.
  DATA: les_usuario TYPE zmm_t_imp_etiq.
  DATA: les_msg TYPE zesd_msg_picking.
  DATA: les_actv TYPE zmm_t_clase_pv.

  DATA:les_cab_ref TYPE zesd_cab_ref.
*.....Rango para fechas de Creación de Documentos
  DATA:    rl_aedat TYPE RANGE OF ekko-aedat,
           ls_aedat LIKE LINE OF rl_aedat.

*... rangos para la clase de documento
  DATA: rg_bsart TYPE RANGE OF ekko-bsart.
  DATA: rl_bsart LIKE LINE OF rg_bsart.
*-------------------------------------------------------------------------------*
* Definición de Tablas Internas
*-------------------------------------------------------------------------------*
*DATA:
  DATA: lti_ekko TYPE STANDARD TABLE OF ltp_ti_ekko.
  DATA: lti_ekpo TYPE STANDARD TABLE OF ltp_ti_ekpo.
  DATA: lti_eket TYPE STANDARD TABLE OF ltp_ti_eket.
  DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
  DATA: lti_msgx TYPE STANDARD TABLE OF zesd_msg_picking.
*-------------------------------------------------------------------------------*
* Definición de Variables
*-------------------------------------------------------------------------------*
  DATA: l_o_ebeln TYPE ekko-ebeln.
  DATA:
*.....Fecha Inicio
           l_e_fecha_inicio TYPE d,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

*{   INSERT         ER3K900279                                        2
*.... Definición de datos detalle
*.....Variable Estructura con detalles del Usuario
  DATA : les_det_usuario TYPE zmm_t_imp_etiq,
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
         lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
         lti_pick_eket TYPE TABLE OF ltp_eket,
         les_pick_eket type ltp_eket,
*.....Variable para Cantidad Pendiente por Picking
         l_e_pend TYPE etmen,
*.....Tabla para Ajustar Clase de Documento de Compras EKKO
*         lti_ekko TYPE TABLE OF ltp_bus_cab,
lti_ubicacion TYPE STANDARD TABLE OF zmm_t_ubicacion,
  les_ubicacion type  zmm_t_ubicacion,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*.....Variable Estructura para Tabla de Mensajes
*         les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
         lti_mara  TYPE TABLE OF mara,
         les_mara  TYPE mara,
*.....Variable Texto Error
         l_e_desc_error TYPE string.

*  DATA: l_o_ebeln TYPE ekko-ebeln.
*
*DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
*DATA: les_actv TYPE  zmm_t_clase_pv.
*... rangos para la clase de documento
*  DATA: rg_bsart TYPE RANGE OF ekko-bsart.
*  DATA: rl_bsart LIKE LINE OF rg_bsart.
* ...
*  DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
*  DATA: les_actv TYPE zmm_t_clase_pv.
*  DATA: lti_msgx TYPE STANDARD TABLE OF zesd_msg_picking.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
                  <lfs_ekpo> TYPE ltp_ti_ekpo,
                  <lfs_eket> TYPE ltp_eket,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking.
*... variable para la unidad de medida
  DATA: lv_meinh            TYPE string. " Unidad de medida tras conversión

*}   INSERT

*... Se valida que haya traído datos la consulta
  IF detalles_usuario IS NOT INITIAL.

*.... Valido que es una consulta directa por documento
    IF i_documento_origen IS NOT INITIAL.
*... convierto el documento de entrada en documento de interno
      MOVE i_documento_origen TO l_o_ebeln.

*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_o_ebeln
        IMPORTING
          output = l_o_ebeln.

*... se consultan las clases de documento asociadas a el tipo de documeto o activadad
      CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
        EXPORTING
          proceso         = 'PICKING'
          cod_modulo      = lc_cod_mod
        TABLES
          tipos_actividad = lti_actv
          t_mensajes      = lti_msgx.
*... se validan que se hayan encontrado registros
      IF lti_actv[] IS NOT INITIAL.
*... se recorre la tabla interna y se llenan los rangos
        LOOP AT lti_actv INTO les_actv.
          rl_bsart-sign = 'I'.
          rl_bsart-option = 'EQ'.
          rl_bsart-low = les_actv-cod_clase.
          APPEND rl_bsart TO  rg_bsart.
        ENDLOOP.

*... se consulta la Info de Cabecera del Pedido de subcontratación
        SELECT SINGLE ebeln bukrs bsart lifnr reswk aedat
          INTO les_ekko
          FROM ekko
          WHERE ebeln EQ l_o_ebeln AND
          bsart IN rg_bsart AND
          loekz EQ '' .  " Indicador de Borrado

        IF sy-subrc EQ 0.
*.... Adiciono el registro encontrado a la tabla interna de cabeceras
          APPEND les_ekko TO lti_ekko.

        ENDIF.
      ELSE.
        les_msg-msg = 'No Existen Clases de Documentos configurados para esa actividad de  Picking'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
      ENDIF.



*.... Valido que es una consulta por fechas
    ELSEIF i_fec_ini_bus IS NOT INITIAL AND i_fec_fin_bus IS NOT INITIAL.
*.....Convierto Fecha_inicio a Formato  Interno
      CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
        EXPORTING
          datum = i_fec_ini_bus
          dtype = 'DATS'
        IMPORTING
          idate = l_e_fecha_inicio.
*.....Convierto Fecha Fin a Formato Interno
      CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
        EXPORTING
          datum = i_fec_fin_bus
          dtype = 'DATS'
        IMPORTING
          idate = l_e_fecha_fin.

*.....Creo Rango de Fechas
      ls_aedat-sign = 'I'.
      ls_aedat-option = 'BT'.
      ls_aedat-low = l_e_fecha_inicio.
      ls_aedat-high = l_e_fecha_fin.
      APPEND ls_aedat TO rl_aedat.

*... se consulta la Info de Cabecera del Pedido de subcontratación
      SELECT ebeln bukrs bsart lifnr reswk aedat
         FROM ekko
            INTO TABLE lti_ekko
               WHERE aedat IN rl_aedat AND
                     bsart EQ 'ZPLA' AND
                     loekz EQ '' .  " Indicador de Borrado.

    ENDIF.
*....  Validamos que se haya encontrado registros de cabeceras
    IF lti_ekko[] IS NOT INITIAL.
*.... Se consultan las posisicones de cada documento
*{   REPLACE        ER3K900279                                        5
*\      SELECT ebeln ebelp matnr bukrs werks lgort menge meins
      SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz
*}   REPLACE
            FROM ekpo
             INTO TABLE lti_ekpo
              FOR ALL ENTRIES IN lti_ekko
               WHERE ebeln EQ lti_ekko-ebeln AND
        elikz EQ '' AND
        loekz EQ '' AND
        retpo EQ 'X' AND
        werks EQ detalles_usuario-centro AND
                     lgort EQ detalles_usuario-almacen.
*... Se valida que se hayan encontrado posiciones
      IF sy-subrc EQ 0.
*... Se consultan el plan de repartos y sus cantidades para validar pendientes de picking
        SELECT ebeln ebelp menge wamng
        FROM eket
         INTO TABLE lti_eket
         FOR ALL ENTRIES IN lti_ekpo
         WHERE ebeln EQ lti_ekpo-ebeln AND
                ebelp EQ lti_ekpo-ebelp.

        IF sy-subrc EQ 0.
          LOOP AT lti_eket INTO les_eket.
*        Si la Cantidad Pendiente = 0, Significa que no Esta Pendiente por Picking (  menge - wamng  )

            les_eket-menge = les_eket-menge - les_eket-wamng.
            IF les_eket-menge EQ 0 .
*              ... borramos la posición que no esta pendiente por picking
              DELETE lti_eket INDEX sy-tabix.
            ENDIF.
          ENDLOOP.


*.....Consulto Información del Proveedor
          SELECT lifnr name1
             FROM lfa1
               INTO TABLE lti_lfa1
                 FOR ALL ENTRIES IN lti_ekko
                   WHERE lifnr EQ lti_ekko-lifnr.



          DATA: lv_index TYPE sy-tabix.
*... Recorro la tabla de cabeceras buscando que almenos haya un registro en la de plan de entregas
          LOOP AT lti_ekko INTO les_ekko.
            lv_index = sy-tabix.
            READ TABLE lti_eket INTO les_eket WITH KEY ebeln = les_ekko-ebeln.
            IF sy-subrc NE 0.
              DELETE lti_ekko INDEX lv_index.
            ELSE.
              les_cab_ref-num_doc     = les_ekko-ebeln.
              les_cab_ref-fecha_crea  = les_ekko-aedat.
              les_cab_ref-tipo_doc    = les_ekko-bsart.

*.....Lectura de Codigo de Proveedor y Codigo de Cliente
              READ TABLE  lti_lfa1 WITH KEY  lifnr = les_ekko-lifnr INTO les_lfa1 .
              IF sy-subrc EQ 0 .
                les_cab_ref-cod_cliente = les_lfa1-lifnr.
                les_cab_ref-nom_cliente = les_lfa1-name1.

              ENDIF.

              APPEND les_cab_ref TO c_det_cabecera.
*{   INSERT         ER3K900279                                        3
              IF I_FLAG_DET eq 'X'.
             IF lti_ekpo is not INITIAL.
*               Extraemos las ubicaciones, cantidades y unidades de medida del material
*               y lo llevamos a la tabla que se exportará
                SELECT material ubicacion UBIC_DEFAULT cantidad unidad_med
                INTO CORRESPONDING FIELDS OF TABLE lti_ubicacion
                FROM zmm_t_ubicacion
                FOR ALL ENTRIES IN lti_ekpo
                WHERE centro   EQ detalles_usuario-centro
                  AND almacen  EQ detalles_usuario-almacen
                  AND material EQ lti_ekpo-MATnr.


            ENDIF.
*                ------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_ekpo into les_ekpo WHERE ebeln = les_ekko-ebeln .
            les_mara-matnr =  les_ekpo-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = c_eans
              t_mara = lti_mara.


*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

SORT lti_ekpo by ebeln matnr ASCENDING.


          LOOP AT lti_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = les_ekko-ebeln.
            CLEAR:les_pick_eket.
*.....Verifico que la Posición este Pendiente por Picking
            READ TABLE lti_eket WITH KEY  ebeln = les_ekko-ebeln ebelp = <lfs_ekpo>-ebelp INTO les_pick_eket.

*            IF les_pick_eket-mng02 = 0.
*              CONTINUE.
*            ENDIF.

*            IF sy-subrc EQ 0 .
*.....Limpieza de Variables
            CLEAR: lti_return, lti_serno, lti_charg, lti_matnr,les_det_pos.
*.....Inserto Registro con Número de Material
            l_e_matnr = <lfs_ekpo>-matnr.
            APPEND l_e_matnr TO lti_matnr.

            READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.

*.....Función para Obtener el la Serie y el Número de Lote para un Material
            CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
              EXPORTING
                werk             = <lfs_ekpo>-werks
                lgort            = <lfs_ekpo>-lgort
                username         = i_usuario
                i_tipo_documento = i_tipo_documento
                reswk            = <lfs_ekko>-reswk
              TABLES
                t_return         = lti_return
                t_serno          = lti_serno     "Serie para el Material
                t_charg          = lti_charg     "Lote para el Material
                t_estatin        = lti_estatin
                t_matnr          = lti_matnr.

            READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico si el material tiene número de serie
              IF lti_serno IS NOT INITIAL .
                APPEND LINES OF lti_serno TO c_seriales.
                <lfs_ekpo>-seriales = 'X'.
              ELSE.
                <lfs_ekpo>-seriales = ' '.
              ENDIF.
*                ENDIF.
*.....Verifico si el Material tiene Lote
              IF lti_charg IS NOT INITIAL.
                APPEND LINES OF lti_charg TO c_lotes.
                <lfs_ekpo>-numero_lote = 'X'.
              ELSE.
                <lfs_ekpo>-numero_lote = ' '.
              ENDIF.
IF I_FLAG_AGRUP is not INITIAL.
  clear:les_det_pos.
                    les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
*                    les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
                    les_det_pos-material = <lfs_ekpo>-matnr.
                    les_det_pos-desc_material = <lfs_ekpo>-txz01.
*                    les_det_pos-cant_por_pick = les_pick_eket-mng02.

                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
                    READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X'.
                      IF sy-subrc eq 0.
                        les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                        ELSE.
                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material.
                          IF sy-subrc eq 0.
                            les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                          ENDIF.
                      ENDIF.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*.....Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_ekpo>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lV_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lV_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
                    les_det_pos-seriales = <lfs_ekpo>-seriales.
                    les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_ekpo>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      les_det_pos-cant_por_pick = les_det_pos-cant_por_pick * les_det_pos-cantidad_ean.
                      ENDIF.
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_ekpo>-matnr cantidad_ean = 1.
                      IF sy-subrc eq 0.
                      les_det_pos-uni_med_doc = <lfs_detalle>-unidad_medida.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.
ELSE.

*.....Limpieza de Variables
              CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                EXPORTING
                  matnr      = <lfs_ekpo>-matnr
                TABLES
                  t_detalle  = lti_detalle
                  t_mensajes = lti_mensajes.

*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
              READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
              IF sy-subrc EQ 0 .
                les_msg-num_doc = <lfs_mensajes>-num_doc.
                les_msg-msg = <lfs_mensajes>-msg.
                les_msg-type_msg = <lfs_mensajes>-msg.
                APPEND les_msg TO c_mensajes.
              ELSE.
*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
                IF lti_detalle IS NOT INITIAL  .
                  LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                    les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
                    les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
                    les_det_pos-material = <lfs_ekpo>-matnr.
                    les_det_pos-desc_material = <lfs_ekpo>-txz01.
*                    les_det_pos-cant_por_pick = les_pick_eket-mng02.
                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
                    READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X'.
                      IF sy-subrc eq 0.
                        les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                        ELSE.
                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material.
                          IF sy-subrc eq 0.
                            les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                          ENDIF.
                      ENDIF.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*.....Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_ekpo>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
                    les_det_pos-seriales = <lfs_ekpo>-seriales.
                    les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*.....Datos de Detalle EAN
                    les_det_pos-ean = <lfs_detalle>-ean.
                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                    APPEND les_det_pos TO c_det_posiciones.
                  ENDLOOP.
                ELSE.
                  les_msg-num_doc = ' '.
                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
                  INTO l_e_desc_error RESPECTING BLANKS.
                  les_msg-msg = l_e_desc_error.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDIF.
              ENDIF.

            ELSE.
              les_msg-num_doc = <lfs_ekpo>-ebeln.
              les_msg-msg = <lfs_return>-message.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
            ENDIF.
*            ELSE.
*              CONTINUE.
*            ENDIF.
          ENDLOOP.

              ENDIF.
*}   INSERT

*.....Ajusto Nombre del Centro
*              SELECT SINGLE name1
*                   FROM t001w
*                     INTO les_cab_ref-nom_cliente
*                        WHERE werks EQ les_ekko-reswk.


            ENDIF.
          ENDLOOP.

        ELSE.
          REFRESH lti_ekko.
          les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
        ENDIF.
      ELSE.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
      ENDIF.
    ELSE.
      les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
    ENDIF.

  ELSE.
    c_mensajes[] = mensajes[].
    RETURN.
  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_documento~get_detalle.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos para devoluciones de proveedor, para el Codigo
*                del Cliente y Número de Cliente se Definio el Centro
*                Suministrador.
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------*
* Definición de Constantes
*-------------------------------------------------------------------------------*
*CONSTANT: ………………….
*……………..
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_cod_mod TYPE c LENGTH 4 VALUE 'DEVO'.
*CONSTANTS : lc_proceso type c
*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_eket,
*{   REPLACE        ER3K900279                                        6
*\          ebeln TYPE ebeln,      "Número del documento de compras
*\          ebelp TYPE ebelp,      "Número de posición del documento de compras
*\          menge TYPE etmen,      "Cantidad de reparto
*\          wamng TYPE wamng,      "Cantidad de salida
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
          mng02 TYPE mng02,
*}   REPLACE
   END OF ltp_eket.

*.....Typo para Busqueda Detalle Cabecera Documento EKKO
  TYPES: BEGIN OF ltp_bus_cab,
*{   REPLACE        ER3K900279                                        3
*\         ebeln TYPE ebeln,
*\         bsart TYPE esart,
         ebeln TYPE ebeln,
         bsart TYPE esart,
         reswk TYPE reswk,
*}   REPLACE
  END OF ltp_bus_cab.

*.....Typo para Busqueda Detalle  Posiciones del Documento EKPO
  TYPES : BEGIN OF ltp_bus_pos,
          ebeln TYPE 	ebeln,   "ekpo
          ebelp	 TYPE ebelp,   "ekpo
          matnr TYPE matnr,    "ekpo
          txz01 TYPE maktx,    "ekpo
          menge TYPE ze_por_picking,   "ekpo
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          meins TYPE  ze_uni_med,      "ekpo
          seriales TYPE	c LENGTH 18,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE  charg_d,   "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          bsart TYPE esart,            "EKKO Clase de documento de compras
*          ean TYPE ean11,              "ZESD_DET_REF Número de artículo europeo (EAN)
*          cantidad_ean  TYPE zed_cantidad_ean, "ZESD_DET_REF Cantidad EAN decimal
*          unidad_med TYPE c LENGTH 3,  "Unidad de medida
    END OF ltp_bus_pos.

*.....Variable Estructura con detalles del Usuario
  DATA : les_det_usuario TYPE zmm_t_imp_etiq,
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
         lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
         lti_pick_eket TYPE TABLE OF ltp_eket,
*{   INSERT         ER3K900279                                        5
          les_pick_eket TYPE ltp_eket,
*}   INSERT
*.....Variable para Cantidad Pendiente por Picking
         l_e_pend TYPE etmen,
*.....Tabla para Ajustar Clase de Documento de Compras EKKO
         lti_ekko TYPE TABLE OF ltp_bus_cab,

*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*.....Variable Estructura para Tabla de Mensajes
         les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
         lti_mara  TYPE TABLE OF mara,
         les_mara  TYPE mara,
*.....Variable Texto Error
         l_e_desc_error TYPE string.

  DATA: l_o_ebeln TYPE ekko-ebeln.
*
*DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
*DATA: les_actv TYPE  zmm_t_clase_pv.
*... rangos para la clase de documento
  DATA: rg_bsart TYPE RANGE OF ekko-bsart.
  DATA: rl_bsart LIKE LINE OF rg_bsart.
* ...
  DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
  DATA: les_actv TYPE zmm_t_clase_pv.
  DATA: lti_msgx TYPE STANDARD TABLE OF zesd_msg_picking.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
                  <lfs_ekpo> TYPE ltp_bus_pos,
                  <lfs_eket> TYPE ltp_eket,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
*{   INSERT         ER3K900279                                        4
                  <lfs_actividad> TYPE zmm_t_clase_pv,
*}   INSERT
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking.
*... variable para la unidad de medida
  DATA: lv_meinh            TYPE string. " Unidad de medida tras conversión
*{   INSERT         ER3K900279                                        8
*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "IDEM
    TABLES
      tipos_actividad = lti_actv
      t_mensajes      = c_mensajes.
*}   INSERT

*.....Valido si Existe Información para el Usuario del Dispositivo
  IF detalles_usuario IS NOT INITIAL.

*... convierto el documento de entrada en documento de interno
    MOVE i_documento_origen TO l_o_ebeln.
*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_o_ebeln
      IMPORTING
        output = l_o_ebeln.

*.....Busqueda de Documentos Pendientes por Picking en tabla de Pos. Documentos de Compras
    SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz
           FROM ekpo
            INTO CORRESPONDING FIELDS OF TABLE lti_pick_ekpo
              WHERE ebeln EQ l_o_ebeln AND
          elikz EQ '' AND
        loekz EQ '' AND
       retpo EQ 'X' AND
                    werks EQ detalles_usuario-centro AND
                    lgort EQ detalles_usuario-almacen.
*.....Verifico que Existan Registros
    IF sy-subrc EQ 0.
*.....Ordeno Tabla para un Mejor Proceso del For All Entries
      SORT lti_pick_ekpo BY ebeln ebelp ASCENDING.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
      LOOP AT lti_pick_ekpo ASSIGNING  <lfs_ekpo>.
        les_mara-matnr = <lfs_ekpo>-matnr.
        APPEND les_mara TO lti_mara.
      ENDLOOP.

*.....Función para Obtener EANs de un Material
      CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
        TABLES
          t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 29/05/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*{   REPLACE        ER3K900279                                        7
*\      SELECT ebeln ebelp menge wamng
      SELECT ebeln ebelp menge wamng mng02
*}   REPLACE
         FROM eket
           INTO TABLE lti_pick_eket
          FOR ALL ENTRIES IN lti_pick_ekpo
          WHERE ebeln EQ lti_pick_ekpo-ebeln AND
                ebelp EQ lti_pick_ekpo-ebelp.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0.
        LOOP AT lti_pick_eket ASSIGNING <lfs_eket>.
          l_e_pend = ( <lfs_eket>-menge  - <lfs_eket>-wamng ).
*.....Si la Cantidad Pendiente = 0, Significa que no Esta Pendiente por Picking
          IF  l_e_pend EQ 0.
            DELETE lti_pick_eket INDEX sy-tabix .
          ELSE.
            CONTINUE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      les_msg-num_doc = i_documento_origen.
      les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

*{   DELETE         ER3K900279                                        1
*\*.....Verifico que Existan Documentos Pendientes por Picking
*\    IF lti_pick_eket IS NOT INITIAL.
*\*.....Ordeno Tabla por Número de Documento
*\      SORT lti_pick_eket BY ebeln ASCENDING.
*\
*\*... se consultan las clases de documento asociadas a el tipo de documeto o activadad
*\      CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
*\        EXPORTING
*\          proceso         = 'PICKING'
*\          cod_modulo      = lc_cod_mod
*\        TABLES
*\          tipos_actividad = lti_actv
*\          t_mensajes      = lti_msgx.
*\*... se validan que se hayan encontrado registros
*\      IF lti_actv[] IS NOT INITIAL.
*\*... se recorre la tabla interna y se llenan los rangos
*\        LOOP AT lti_actv INTO les_actv.
*\          rl_bsart-sign = 'I'.
*\          rl_bsart-option = 'EQ'.
*\          rl_bsart-low = les_actv-cod_clase.
*\          APPEND rl_bsart TO  rg_bsart.
*\        ENDLOOP.
*\
*\
*\*.....Busco Clase de Documento de Compras para Documentos Pendientes por Picking
*\
*\        SELECT ebeln bsart
*\           FROM  ekko
*\             INTO TABLE lti_ekko
*\              FOR ALL ENTRIES IN lti_pick_eket
*\                WHERE ebeln EQ lti_pick_eket-ebeln
*\                 AND bsart IN rg_bsart.
*\*.....Verifico que Existan Registros
*\        IF sy-subrc EQ 0.
*\
*\          LOOP AT lti_pick_ekpo ASSIGNING  <lfs_ekpo>.
*\*.....Verifico que la Posición este Pendiente por Picking
*\            READ TABLE lti_pick_eket WITH KEY  ebeln = <lfs_ekpo>-ebeln
*\                                               ebelp = <lfs_ekpo>-ebelp TRANSPORTING NO FIELDS.
*\
*\            IF sy-subrc EQ 0.
*\*.....Ajusto a la Posición su Clase de Documento
*\              READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.
*\              IF sy-subrc EQ 0.
*\                <lfs_ekpo>-bsart = <lfs_ekko>-bsart.
*\*.....Limpieza de Variables
*\            CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*\*.....Inserto Registro con Número de Material
*\            l_e_matnr = <lfs_ekpo>-matnr.
*\            APPEND l_e_matnr TO lti_matnr.
*\*.....Función que Retorna Serie y Lote para Un Material
*\                CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
*\                  EXPORTING
*\                    werk      = <lfs_ekpo>-werks
*\                    lgort     = <lfs_ekpo>-lgort
*\                    username  = i_usuario
*\                  TABLES
*\                    t_return  = lti_return
*\                    t_serno   = lti_serno     "Serie para el Material
*\                    t_charg   = lti_charg     "Lote para el Material
*\                    t_estatin = lti_estatin
*\                    t_matnr   = lti_matnr.
*\*.....Verifico que no Existan Errores
*\              READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
*\              IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*\*.....Verifico que el Material Tenga Número de Serie
*\                IF lti_serno IS NOT INITIAL.
*\                  APPEND LINES OF lti_serno TO c_seriales.
*\                  <lfs_ekpo>-seriales = 'X'.
*\                ELSE.
*\                  <lfs_ekpo>-seriales = ' '.
*\                ENDIF.
*\*                ENDIF.
*\*.....Verifico si el Material Tiene Número de Lote
*\                IF lti_charg  IS NOT INITIAL  .
*\                  APPEND LINES OF lti_charg TO c_lotes.
*\                  <lfs_ekpo>-numero_lote = 'X'.
*\                ELSE.
*\                  <lfs_ekpo>-numero_lote = ' '.
*\                ENDIF.
*\              ENDIF.
*\*.....Limpieza de Variables
*\              CLEAR : lti_detalle, lti_mensajes.
*\*.....Función para Obtener EANs de un Material
*\                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                  EXPORTING
*\                    matnr      = <lfs_ekpo>-matnr
*\                  TABLES
*\                    t_detalle  = lti_detalle
*\                    t_mensajes = lti_mensajes.
*\
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
*\                READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
*\                IF sy-subrc EQ 0 .
*\                  les_msg-num_doc = <lfs_mensajes>-num_doc.
*\                  les_msg-msg = <lfs_mensajes>-msg.
*\                  les_msg-type_msg = <lfs_mensajes>-msg.
*\                  APPEND les_msg TO c_mensajes.
*\                ELSE.
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\                  IF lti_detalle IS NOT INITIAL  .
*\                    LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                      les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
*\                      les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
*\                      les_det_pos-material = <lfs_ekpo>-matnr.
*\                      les_det_pos-desc_material = <lfs_ekpo>-txz01.
*\                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
*\                      les_det_pos-ubic_fija = ' '.
*\                      les_det_pos-ubic_tmp =  ' '.
*\                      les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\
*\
*\*       Realizar la conversión de la unidad de medida
*\                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                        EXPORTING
*\                          input          = <lfs_ekpo>-meins
*\                          language       = 'S'
*\                        IMPORTING
*\                          output         = lv_meinh
*\                        EXCEPTIONS
*\                          unit_not_found = 1
*\                          OTHERS         = 2.
*\                      IF lv_meinh EQ '004'.
*\                        les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\                      ELSE.
*\                        les_det_pos-uni_med_doc = lv_meinh.
*\                      ENDIF.
*\                      les_det_pos-seriales = <lfs_ekpo>-seriales.
*\                      les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*\*.....Datos de Detalle EAN
*\                      les_det_pos-ean = <lfs_detalle>-ean.
*\                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                      APPEND les_det_pos TO c_det_posiciones.
*\                    ENDLOOP.
*\                  ELSE.
*\                    les_msg-num_doc = ' '.
*\                    CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
*\                    INTO l_e_desc_error SEPARATED BY space.
*\                    les_msg-msg = l_e_desc_error.
*\                    les_msg-type_msg = 'E'.
*\                    APPEND les_msg TO c_mensajes.
*\                  ENDIF.
*\                ENDIF.
*\*              else.
*\*                les_msg-num_doc = ' '.
*\*                les_msg-msg = 'No Existen Documentos'.
*\*                les_msg-type_msg = 'E'.
*\*                append les_msg to t_mensajes.
*\*              endif.
*\              ENDIF.
*\            ELSE.
*\              CONTINUE.
*\            ENDIF.
*\          ENDLOOP.
*\        ENDIF.
*\      ENDIF.
*\
*\
*\    ELSE.
*\      les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
*\      les_msg-type_msg = 'E'.
*\      APPEND les_msg TO c_mensajes.
*\      RETURN.
*\    ENDIF.
*\  ELSE.
*\    c_mensajes[] = mensajes[].
*\    RETURN.
*\  ENDIF.
*\ENDMETHOD.
*}   DELETE
*{   INSERT         ER3K900279                                        2
*.....Verifico que Existan Documentos Pendientes por Picking
      IF lti_pick_eket IS NOT INITIAL.
*.....Ordeno Tabla por Número de Documento
        SORT lti_pick_eket BY ebeln ASCENDING.
*Ordeno la Tabla
        SORT lti_pick_ekpo BY ebeln  ASCENDING.
*.....Ajusto Tabla de Posiciones con su Clase de Documento Respectiva
        SELECT ebeln bsart reswk
             FROM ekko
           INTO TABLE lti_ekko
                    FOR ALL ENTRIES IN lti_pick_ekpo
                            WHERE ebeln EQ lti_pick_ekpo-ebeln AND
                                  bsart EQ i_clase_documento.

*.....Busco el Codigo de la Clase para El documento Recibido
        READ TABLE lti_actv WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900304                                        1
*\          LOOP AT  lti_ekko ASSIGNING <lfs_ekko>  .
*\            DELETE lti_ekko WHERE bsart NE <lfs_actividad>-cod_clase.
*\          ENDLOOP.
*          LOOP AT  lti_ekko ASSIGNING <lfs_ekko>  .
            DELETE lti_ekko WHERE bsart NE <lfs_actividad>-cod_clase.
*          ENDLOOP.
*}   REPLACE
        ELSE.
          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.

*.....Ajusto Información de la Tabla de Posiciones
*{   DELETE         ER3K900304                                        2
*\        LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo>.
*\          READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.
*\          IF sy-subrc EQ 0 .
*\            <lfs_ekpo>-bsart =  <lfs_ekko>-bsart.
*\          ENDIF.
*\        ENDLOOP.
*}   DELETE

*.....Verifico que la Posición este Pendiente por Picking
        READ TABLE lti_pick_eket WITH KEY  ebeln = l_o_ebeln "i_documento_origen
                                 ASSIGNING  <lfs_eket>.
        IF sy-subrc EQ 0.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .
            les_mara-matnr =  <lfs_ekpo>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*{   REPLACE        ER3K900304                                        3
*\*  .....Función para Obtener EANs de un Material
*\          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\            TABLES
*\              t_mara = lti_mara.
*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = c_eans
              t_mara = lti_mara.

*}   REPLACE
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*{   INSERT         ER3K900304                                        4
SORT lti_pick_ekpo by ebeln matnr ASCENDING.
*}   INSERT

          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .
            CLEAR:les_pick_eket.
*.....Verifico que la Posición este Pendiente por Picking
            READ TABLE lti_pick_eket WITH KEY  ebeln = l_o_ebeln ebelp = <lfs_ekpo>-ebelp INTO les_pick_eket.

*            IF les_pick_eket-mng02 = 0.
*              CONTINUE.
*            ENDIF.

*            IF sy-subrc EQ 0 .
*.....Limpieza de Variables
            CLEAR: lti_return, lti_serno, lti_charg, lti_matnr,les_det_pos.
*.....Inserto Registro con Número de Material
            l_e_matnr = <lfs_ekpo>-matnr.
            APPEND l_e_matnr TO lti_matnr.

            READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.

*.....Función para Obtener el la Serie y el Número de Lote para un Material
            CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
              EXPORTING
                werk             = <lfs_ekpo>-werks
                lgort            = <lfs_ekpo>-lgort
                username         = i_usuario
                i_tipo_documento = i_tipo_documento
                reswk            = <lfs_ekko>-reswk
              TABLES
                t_return         = lti_return
                t_serno          = lti_serno     "Serie para el Material
                t_charg          = lti_charg     "Lote para el Material
                t_estatin        = lti_estatin
                t_matnr          = lti_matnr.

            READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico si el material tiene número de serie
              IF lti_serno IS NOT INITIAL .
                APPEND LINES OF lti_serno TO c_seriales.
                <lfs_ekpo>-seriales = 'X'.
              ELSE.
                <lfs_ekpo>-seriales = ' '.
              ENDIF.
*                ENDIF.
*.....Verifico si el Material tiene Lote
              IF lti_charg IS NOT INITIAL.
                APPEND LINES OF lti_charg TO c_lotes.
                <lfs_ekpo>-numero_lote = 'X'.
              ELSE.
                <lfs_ekpo>-numero_lote = ' '.
              ENDIF.
*                ENDIF.
*{   REPLACE        ER3K900304                                        5
*\*.....Limpieza de Variables
*\              CLEAR : lti_detalle, lti_mensajes.
*\*.....Función para Obtener EANs de un Material
*\              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                EXPORTING
*\                  matnr      = <lfs_ekpo>-matnr
*\                TABLES
*\                  t_detalle  = lti_detalle
*\                  t_mensajes = lti_mensajes.
*\
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
*\              READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
*\              IF sy-subrc EQ 0 .
*\                les_msg-num_doc = <lfs_mensajes>-num_doc.
*\                les_msg-msg = <lfs_mensajes>-msg.
*\                les_msg-type_msg = <lfs_mensajes>-msg.
*\                APPEND les_msg TO c_mensajes.
*\              ELSE.
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\                IF lti_detalle IS NOT INITIAL  .
*\                  LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                    les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
*\                    les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
*\                    les_det_pos-material = <lfs_ekpo>-matnr.
*\                    les_det_pos-desc_material = <lfs_ekpo>-txz01.
*\                    les_det_pos-cant_por_pick = les_pick_eket-mng02.
*\*                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
*\                    les_det_pos-ubic_fija = ' '.
*\                    les_det_pos-ubic_tmp =  ' '.
*\                    les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\*.....Realizar la conversión de la unidad de medida
*\                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                      EXPORTING
*\                        input          = <lfs_ekpo>-meins
*\                        language       = 'S'
*\                      IMPORTING
*\                        output         = l_e_meinh
*\                      EXCEPTIONS
*\                        unit_not_found = 1
*\                        OTHERS         = 2.
*\                    IF l_e_meinh EQ '004'.
*\                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\                    ELSE.
*\                      les_det_pos-uni_med_doc = l_e_meinh.
*\                    ENDIF.
*\                    les_det_pos-seriales = <lfs_ekpo>-seriales.
*\                    les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*\*.....Datos de Detalle EAN
*\                    les_det_pos-ean = <lfs_detalle>-ean.
*\                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                    APPEND les_det_pos TO c_det_posiciones.
*\                  ENDLOOP.
*\                ELSE.
*\                  les_msg-num_doc = ' '.
*\                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
*\                  INTO l_e_desc_error RESPECTING BLANKS.
*\                  les_msg-msg = l_e_desc_error.
*\                  les_msg-type_msg = 'E'.
*\                  APPEND les_msg TO c_mensajes.
*\                ENDIF.
*\              ENDIF.
IF I_FLAG_AGRUP is not INITIAL.
  clear:les_det_pos.
                    les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
*                    les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
                    les_det_pos-material = <lfs_ekpo>-matnr.
                    les_det_pos-desc_material = <lfs_ekpo>-txz01.
*                    les_det_pos-cant_por_pick = les_pick_eket-mng02.
                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*.....Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_ekpo>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lV_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lV_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
                    les_det_pos-seriales = <lfs_ekpo>-seriales.
                    les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_ekpo>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.
ELSE.

*.....Limpieza de Variables
              CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                EXPORTING
                  matnr      = <lfs_ekpo>-matnr
                TABLES
                  t_detalle  = lti_detalle
                  t_mensajes = lti_mensajes.

*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
              READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
              IF sy-subrc EQ 0 .
                les_msg-num_doc = <lfs_mensajes>-num_doc.
                les_msg-msg = <lfs_mensajes>-msg.
                les_msg-type_msg = <lfs_mensajes>-msg.
                APPEND les_msg TO c_mensajes.
              ELSE.
*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
                IF lti_detalle IS NOT INITIAL  .
                  LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                    les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
                    les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
                    les_det_pos-material = <lfs_ekpo>-matnr.
                    les_det_pos-desc_material = <lfs_ekpo>-txz01.
*                    les_det_pos-cant_por_pick = les_pick_eket-mng02.
                      les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*.....Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_ekpo>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
                    les_det_pos-seriales = <lfs_ekpo>-seriales.
                    les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
*.....Datos de Detalle EAN
                    les_det_pos-ean = <lfs_detalle>-ean.
                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                    APPEND les_det_pos TO c_det_posiciones.
                  ENDLOOP.
                ELSE.
                  les_msg-num_doc = ' '.
                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
                  INTO l_e_desc_error RESPECTING BLANKS.
                  les_msg-msg = l_e_desc_error.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDIF.
              ENDIF.
*}   REPLACE
            ELSE.
              les_msg-num_doc = <lfs_ekpo>-ebeln.
              les_msg-msg = <lfs_return>-message.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
            ENDIF.
*            ELSE.
*              CONTINUE.
*            ENDIF.
          ENDLOOP.
        ELSE.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'No Esta Pendiente por Picking'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'No Esta Pendiente por Picking'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
      ELSE.
      c_mensajes[] = mensajes[].
      RETURN.
      ENDIF.
ENDMETHOD.

*}   INSERT


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
*\les_ubicacion_suge-cantidad  = les_ubicacion-cantidad.
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
*\  endif.
*... Valido que hayan datos
  if not i_t_matnr[] is initial.
*... Consulto las ubicaciones para todos los materiales
        select *
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
  endif.
*}   REPLACE

endif.
endmethod.


method ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Carga los datos del documento
* Autor Prog.  :
* Fecha Creac. : 26.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Metodo para Setiar Atributos de compras
  call method me->zif_lgtica_documento~set_compras
    exporting
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Metodo para Setiar Atributos de entregas
  call method me->zif_lgtica_documento~SET_ENTREGAS
    exporting
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Indicador para Proceso que Genera Entrega
   c_indicador = 'X'.
endmethod.


METHOD zif_lgtica_pickable~get_header_picking.
*CALL METHOD SUPER->ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING
**  EXPORTING
**    i_header_ventas  =
**    i_header_compras =
*  RECEIVING
*    R_HEADER_PICKING =
*    .
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Genara Estructura de Cabecera para un documento de picking
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* Compras
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de compras
*{   REPLACE        ER3K900309                                        1
*\  DATA: les_compras TYPE ekko.
  DATA: les_compras TYPE ZESD_CAB_CMP.
*}   REPLACE
*.... Estructura para el manejo del picking
  DATA: les_header_picking TYPE zedsd_picking.

*.... Estructuras para el manejo de las entregas
  DATA: lti_entregas TYPE STANDARD TABLE OF zesd_det_ent_pkg,
        les_entregas TYPE zesd_det_ent_pkg.

*.... Copio el atributo de las entregas a la estructura
  MOVE zif_lgtica_documento~t_entregas TO lti_entregas.


  READ TABLE cabeceras_compras INTO les_compras INDEX 1.

  IF sy-subrc EQ 0.
    les_header_picking-mandt = sy-mandt.
*les_header_picking-PICKNUM =
*les_header_picking-PROCESO =
*les_header_picking-SUBPROCESO =
*les_header_picking-VBELN =
    les_header_picking-ebeln = les_compras-ebeln.
*les_header_picking-AGLUTINADOR =
*les_header_picking-VBELN2 =
    les_header_picking-fecha = sy-datum.
    les_header_picking-hora = sy-uzeit.
    les_header_picking-usuario = detalles_usuario-usuario.
    les_header_picking-uname = sy-uname.
    les_header_picking-femod = sy-datum.
    les_header_picking-homod = sy-uzeit.
    les_header_picking-usrmod = detalles_usuario-usuario.
    les_header_picking-unamod = sy-uname.
*les_header_picking-ESTADO =

    LOOP AT lti_entregas INTO les_entregas.
      les_header_picking-vbeln2 = les_entregas-entrega.
      APPEND les_header_picking TO r_header_picking.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


method ZIF_LGTICA_PICKABLE~GET_POS_PICKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Genara Estructura de detalle para un documento de picking
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* compras
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de compras detalle
  data: les_compras_dt type ZESD_POSICIONES_COMPRAS.

*.... Estructuras para el manejo de la tabla de entregas sap
  data: lti_lips type STANDARD TABLE OF lips.
  data: les_lips type lips.
*.... Estructura para el manejo del picking detalle
  data: les_pos_picking type ZEDSD_PICKING_DET.

*.... Estructuras para el manejo de las entregas
  data: lti_entregas type standard table of zesd_det_ent_pkg,
        les_entregas type zesd_det_ent_pkg.

*.... Copio el atributo de las entregas a la estructura
  move zif_lgtica_documento~t_entregas to lti_entregas.

*.... Selecciono todos los materiales de la entrega
  SELECT * FROM lips
           INTO TABLE lti_lips
           FOR ALL ENTRIES IN lti_entregas
           WHERE vbeln EQ lti_entregas-entrega
           ORDER BY PRIMARY KEY.
*.... recorro el atributo de entregas del documento
  loop at lti_entregas into les_entregas.

      LOOP AT lti_lips into les_lips where VBELN = les_entregas-entrega.
          clear:les_pos_picking,les_compras_dt.
*.... Leo la detalle de compras
          read table DETALLES_compras into les_compras_dt with key matnr = les_lips-matnr EBELP = les_lips-KDPOS .


          les_pos_picking-MATERIAL = les_lips-matnr.
          les_pos_picking-POSICION = les_lips-POSNR.
*{   REPLACE        ER3K900309                                        1
*\          les_pos_picking-CANTIDAD = les_lips-LFIMG.
*\          les_pos_picking-UMC = les_lips-MEINS.
          les_pos_picking-CANTIDAD = les_lips-LGMNG.

          les_pos_picking-UMC = les_lips-MEINS. " unidad de medida base
*}   REPLACE
*          les_pos_picking-DIFERENCIA
*          les_pos_picking-UMD
*          les_pos_picking-UBICACION_TMP
*          les_pos_picking-UBICACION_SU
          les_pos_picking-FECHA = sy-datum.
          les_pos_picking-HORA = sy-uzeit.
*          les_pos_picking-CONTEO
*          les_pos_picking-CANTCONT
*          les_pos_picking-UMCC
*          les_pos_picking-P_CONFIRM
*          les_pos_picking-DOC_ASOCIADO
          les_pos_picking-VBELN2 = les_lips-vbeln.
          collect les_pos_picking into r_pos_picking.
      ENDLOOP.
  endloop.
SORT r_pos_picking by posicion.
  DELETE adjacent duplicates from r_pos_picking comparing VBELN2 MATERIAL posicion.
endmethod.
ENDCLASS.
