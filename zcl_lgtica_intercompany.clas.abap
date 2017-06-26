class ZCL_LGTICA_INTERCOMPANY definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_INTERCOMPANY
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
*"* protected components of class ZCL_LGTICA_INTERCOMPANY
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_INTERCOMPANY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_INTERCOMPANY IMPLEMENTATION.


METHOD zif_lgtica_documento~generar_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Entregas para Proceso Intercompany
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
          EBELP type EBELP,
          vstel TYPE vstel,
          lprio TYPE lprio,
          kunnr TYPE kunnr,
          route TYPE route,
          vkorg TYPE vkorg,
          vtweg TYPE vtweg,
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
*.....Estructura para Entregas
        les_msg TYPE zesd_msg_picking,
*.....Variable para Mensajes
        l_e_msg TYPE string,
*.....Variable Fecha
        l_e_fecha TYPE sy-datum,
*{   INSERT         ER3K900309                                        7
        e_syst type syst,
        number type SYMSGNO,
*}   INSERT

        lo_pick TYPE REF TO zif_lgtica_pickable,
        lo_pick_clas TYPE REF TO zcl_lgtica_picking.

*.....Field Simbols para Procesamiento de Información de Compras
  FIELD-SYMBOLS: <lfs_ekpo> TYPE zesd_posiciones_compras,
*{   REPLACE        ER3K900309                                       10
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

    SELECT ebeln EBELP vstel lprio kunnr route vkorg  vtweg
       FROM ekpv
         INTO TABLE lti_ekpv
           FOR ALL ENTRIES IN detalles_compras
            WHERE ebeln EQ detalles_compras-ebeln.

    IF sy-subrc EQ 0 .
      DATA: dt_compras type STANDARD TABLE OF ZESD_POSICIONES_COMPRAS.
      dt_compras = detalles_compras.

      DELETE dt_compras WHERE loekz ne ' '.
*.....Verifico el Número de Entregas a Generar

      LOOP AT dt_compras  ASSIGNING <lfs_ekpo> where loekz eq ' '
*                                WHERE werks EQ detalles_usuario-centro.
        .
*.....Para el Centro Generar la Entrega
        AT END OF werks.

          l_e_fecha = sy-datum.

          CLEAR :les_key_enque_read, les_vetvg, lti_key_enque_read, lti_vetvg, les_entregas,
                 lti_vbls.

*.....Busco la Fecha de Creacíon del Documento
          READ TABLE cabeceras_compras WITH KEY ebeln = <lfs_ekpo>-ebeln
                                       ASSIGNING <lfs_ekko>.
          READ TABLE lti_ekpv INDEX 1 ASSIGNING <lfs_ekpv>.

          IF sy-subrc EQ 0 .
            les_key_enque_read-panum = '1'.
            les_key_enque_read-vbobj = 'B'.
            les_key_enque_read-vbtyp = 'V'.
            les_key_enque_read-vbeln = <lfs_ekko>-ebeln.
            les_key_enque_read-id = '1'.
            les_key_enque_read-kzazu = 'X'.
            les_key_enque_read-ledat = l_e_fecha. "<lfs_ekko>-aedat.
            les_key_enque_read-tabix = '1'.
            READ TABLE lti_ekpv WITH KEY ebeln = <lfs_ekko>-ebeln EBELP = <lfs_ekpo>-ebelp ASSIGNING <lfs_ekpv>.
            IF  sy-subrc EQ 0 .
              les_key_enque_read-vstel = <lfs_ekpv>-vstel.
              les_key_enque_read-kunwe = <lfs_ekpv>-kunnr.
            ENDIF.

            APPEND les_key_enque_read TO lti_key_enque_read.

            les_vetvg-vstel = <lfs_ekpv>-vstel.
            les_vetvg-ledat = <lfs_ekko>-aedat.
            les_vetvg-lprio = <lfs_ekpv>-lprio.
            les_vetvg-kunwe = <lfs_ekpv>-kunnr.
            les_vetvg-vbeln = <lfs_ekko>-ebeln.
            les_vetvg-vkorg = <lfs_ekpv>-vkorg.
            les_vetvg-vtweg = <lfs_ekpv>-vtweg.
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
*{   REPLACE        ER3K900309                                        2
*\                e_vbsk            = lti_vbsk.
                e_vbsk            = lti_vbsk
                e_syst            = e_syst.
*}   REPLACE
*.....Ordeno Tabla
            SORT lti_vbls BY vbeln_lif posnr_lif ASCENDING.
*.....Elimino Registros Duplicados
*          DELETE ADJACENT DUPLICATES FROM lti_vbls COMPARING vbeln_lif posnr_lif.
            IF lti_vbls[] IS NOT INITIAL .
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
*{   REPLACE        ER3K900309                                        3
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
*{   REPLACE        ER3K900309                                        4
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
                    les_msg-msg = l_e_msg.
*{   REPLACE        ER3K900309                                        8
*\                    les_msg-type_msg = les_bapiret1-type.
                    les_msg-type_msg = 'E'.
*}   REPLACE
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
                  ENDLOOP.
                  CLEAR : lti_vbfs.
*{   INSERT         ER3K900309                                        5
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
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
*}   INSERT
                ENDIF.
              ELSE.
*{   REPLACE        ER3K900309                                        6
*\                CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln space
*\                          ' Centro Nro' space <lfs_ekpo>-werks space  'Verifique Información'
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
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
*                CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln space
*                          ' Centro Nro' space <lfs_ekpo>-werks space  'Verifique Información'
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
* Descripción  : Cabecera de Documentos para InterCompany
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
*{   INSERT         ER3K900279                                        1
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
          meins TYPE ze_uni_med,       "ekpo
          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          retpo	TYPE retpo,            "Posición de devolución
          bsart TYPE esart,            "EKKO Clase de documento de compras
    END OF ltp_bus_pos.

*}   INSERT
*.....Typo para Busqueda de Documento Pendiente por Picking (EKPO)
  TYPES : BEGIN OF ltp_ekpo,
*{   REPLACE        ER3K900279                                        3
*\          ebeln TYPE ebeln,      "Número del documento de compras
*\          ebelp TYPE ebelp,      "Número de posición del documento de compras
*\          loekz TYPE eloek,      "Indicador de borrado en el documento de compras
*\          elikz TYPE elikz,      "Indicador de entrega final
          ebeln TYPE 	ebeln,   "ekpo
          ebelp	 TYPE ebelp,   "ekpo
          matnr TYPE matnr,    "ekpo
          txz01 TYPE maktx,    "ekpo
          menge TYPE ze_por_picking,   "ekpo
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          meins TYPE ze_uni_med,       "ekpo
          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          retpo	TYPE retpo,            "Posición de devolución
          bsart TYPE esart,            "EKKO Clase de documento de compras          ebeln TYPE ebeln,      "Número del documento de compras

*}   REPLACE
          END OF ltp_ekpo.

*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_eket,
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
   END OF ltp_eket.

*.....Typo para Busqueda de Documento EKKO
  TYPES : BEGIN OF ltp_bus_cab,
          ebeln TYPE ebeln,     "Número del documento de compras
          bsart TYPE esart,     "Clase de documento de compras
          aedat TYPE erdat,     "Fecha de creación del registro
          lifnr TYPE elifn,     "Número de cuenta del proveedor
         END OF ltp_bus_cab.

*.....Typo para Número y Nombre de Proveedor
  TYPES : BEGIN OF ltp_comp_cab,
          lifnr TYPE lifnr,      "Número de cuenta del proveedor o acreedor
          name1 TYPE name1_gp,                              "Nombre 1
        END OF ltp_comp_cab.
*{   INSERT         ER3K900279                                        2
  DATA :
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
*           lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
*           lti_pick_eket TYPE TABLE OF ltp_eket,
*.....Variable para Cantidad Pendiente por Picking
*           l_e_pend TYPE etmen,
*.....Tabla para Ajustar Clase de Documento de Compras EKKO
           lti_ekko TYPE TABLE OF ltp_bus_cab,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Variable Estructura para Tabla de Mensajes
*           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*           lv_ebeln TYPE ebeln,
*.....Tabla Internas de Retorno al Consultar la Actividad
*        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara  TYPE mara.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
*                  <lfs_ekpo> TYPE ltp_bus_pos,
*                  <lfs_eket> TYPE ltp_eket,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking.
*                  <lfs_actividad> TYPE zmm_t_clase_pv.

*}   INSERT

  DATA :
*.....Variable Estructura con Información del Documento
           les_cab_ref TYPE zesd_cab_ref,
*.....Variable Tabla Interna con Documentos Pendientes por Picking EKPO
           lti_pick_ekpo TYPE TABLE OF ltp_ekpo,
*.....Variable Tabla Interna con Documentos Pendientes por Picking EKET
           lti_pick_eket TYPE TABLE OF ltp_eket,
*.....Variable para Cantidad Pendiente por Picking
           l_e_pend TYPE etmen,
*.....Variable Tabla Interna con Detalle de Cabecera de Documentos
           lti_bus_cab TYPE TABLE OF ltp_bus_cab,
*.....Variable Estructura para con Detalle de Cabecera
           les_bus_cab TYPE ltp_bus_cab,
*.....Variable para Codigo y Nombre de Proveedor
           lti_comp_cab TYPE TABLE OF ltp_comp_cab,
*.....Variable Estructura para con Detalle de Cabecera  (Centro)
           les_comp_cab TYPE ltp_comp_cab,
*.....Variables Fecha de Creación de Documento
           l_e_ini_crea TYPE ekko-aedat,
           l_e_fin_crea TYPE ekko-aedat,
*.....Rango para fechas de Creación de Documentos
           rl_aedat TYPE RANGE OF ekko-aedat,
           ls_aedat LIKE LINE OF rl_aedat,
*.....Variable Estructura para Mensajes
           les_msg TYPE zesd_msg_picking,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Fecha Inicio
           l_e_fecha_inicio TYPE d,

           lv_ebeln TYPE ebeln,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS :<lfs_ekpo> TYPE ltp_ekpo,
                 <lfs_eket> TYPE ltp_eket,
                 <lfs_bus_cab> TYPE ltp_bus_cab,
                 <lfs_comp_cab> TYPE ltp_comp_cab,
                  <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "ICOM
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.
      IF i_documento_origen IS NOT INITIAL .
        MOVE i_documento_origen TO lv_ebeln.

*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_ebeln
          IMPORTING
            output = lv_ebeln.
*.... Busqueda Cabecera del documento de compras validando el centro suministrador
        SELECT ebeln bsart aedat lifnr
                FROM ekko
                INTO TABLE lti_bus_cab
                WHERE ebeln EQ lv_ebeln AND
                      reswk EQ detalles_usuario-centro AND
                      loekz EQ ' '.
        IF sy-subrc EQ 0.
*.....Busqueda de Pedidos de Venta para El Usuario Según el Centro, Almacen y Documento Origne que tengan Asignados
*{   REPLACE        ER3K900279                                        4
*\          SELECT ebeln ebelp loekz elikz
          SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz retpo
*}   REPLACE
                 FROM ekpo
*{   REPLACE        ER3K900279                                        5
*\                 INTO TABLE lti_pick_ekpo
                 INTO CORRESPONDING FIELDS OF TABLE lti_pick_ekpo
*}   REPLACE
                 FOR ALL ENTRIES IN lti_bus_cab
                 WHERE ebeln EQ lti_bus_cab-ebeln AND
                       retpo  EQ ' ' AND
                       loekz EQ ' '.
        ENDIF.


      ELSE.
*.... Busqueda Cabecera del documento de compras validando el centro suministrador
        SELECT ebeln bsart aedat lifnr
                FROM ekko
                INTO TABLE lti_bus_cab
                WHERE reswk EQ detalles_usuario-centro AND
                      loekz EQ ' '.
        IF sy-subrc EQ 0.
*.....Busqueda de Pedidos de Venta para El Usuario Según el Centro y Almacen que tengan Asignados
*{   REPLACE        ER3K900279                                        6
*\          SELECT ebeln ebelp loekz elikz
SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz retpo
*}   REPLACE
                 FROM ekpo
*{   REPLACE        ER3K900279                                        7
*\                 INTO TABLE lti_pick_ekpo
                 INTO CORRESPONDING FIELDS OF TABLE lti_pick_ekpo
*}   REPLACE
                 FOR ALL ENTRIES IN lti_bus_cab
                 WHERE ebeln EQ lti_bus_cab-ebeln AND
                       retpo  EQ ' ' AND
                       loekz EQ ' '.
        ENDIF.
      ENDIF.
*.....Verifico que Existan Registros
      IF lti_pick_ekpo[] IS NOT INITIAL.
        CLEAR:lti_bus_cab.
*.....Ordeno Tabla Interna para Mejor Procesamiento en el For All Entries
        SORT lti_pick_ekpo BY ebeln ebelp ASCENDING.

        SELECT ebeln ebelp menge wamng
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
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.

*.....Verifico que Existan Registros Pendientes por Picking
      IF lti_pick_eket IS NOT INITIAL .
*......Verifico Parametros de Entrada de la Función
        IF i_fec_ini_bus IS NOT INITIAL AND i_fec_fin_bus IS NOT INITIAL.
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

*.....Ordeno la Tabla para un Mejor Proceso en el For All Entriew
          SORT  lti_pick_eket BY  ebeln ebelp ASCENDING.
*.....Busco Información para la Clase de Documento
          READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
          IF sy-subrc EQ 0 .
*.....Busco Informacion de Cabecera del Pedido
            SELECT ebeln bsart aedat lifnr
               FROM ekko
                  INTO TABLE lti_bus_cab
                       FOR ALL ENTRIES IN lti_pick_eket
                     WHERE ebeln EQ lti_pick_eket-ebeln AND
                           aedat IN rl_aedat AND
                           bsart EQ <lfs_actividad>-cod_clase.       "EDIM
            IF sy-subrc NE 0 .
              les_msg-msg =  'No Existe Informacion con los Datos Ingresados'.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
              RETURN.
            ENDIF.
          ELSE.
            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.

*.....Verifico que Existan Registros
          IF sy-subrc EQ 0.
*.....Ordeno la Tabla para un Mejor Proceso en el For All Entries
            SORT lti_bus_cab BY ebeln ASCENDING.

*.....Consulto Información del Proveedor
            SELECT lifnr name1
               FROM lfa1
                 INTO TABLE lti_comp_cab
                   FOR ALL ENTRIES IN lti_bus_cab
                     WHERE lifnr EQ lti_bus_cab-lifnr.
          ENDIF.
*.....Recorro Tabla Con Detalle de Cabecera de los Documentos Pendientes por Picking
          LOOP AT lti_bus_cab ASSIGNING <lfs_bus_cab>.
*.....Ajusto el Nombre del Proveedor
            READ TABLE lti_comp_cab WITH KEY  lifnr = <lfs_bus_cab>-lifnr
                                    ASSIGNING <lfs_comp_cab>.
            IF sy-subrc EQ 0.
              les_cab_ref-num_doc     = <lfs_bus_cab>-ebeln.
              les_cab_ref-fecha_crea  = <lfs_bus_cab>-aedat.
              les_cab_ref-tipo_doc    = <lfs_bus_cab>-bsart.
              les_cab_ref-cod_cliente = <lfs_bus_cab>-lifnr.
              les_cab_ref-nom_cliente = <lfs_comp_cab>-name1.
              APPEND les_cab_ref TO c_det_cabecera.
*{   INSERT         ER3K900279                                        8
             IF I_FLAG_DET eq 'X'.
*.....Verifico que la Posición este Pendiente por Picking
        READ TABLE lti_pick_eket WITH KEY  ebeln = lv_ebeln "i_documento_origen
                                           ASSIGNING <lfs_eket>.
        IF sy-subrc EQ 0.
*-----------------------------------------------------------------------
*.......MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*       Modificado por: Andrés Felipe Castro.
          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .
            les_mara-matnr = <lfs_ekpo>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_mara = lti_mara.
*.......FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .

            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_ekpo>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_ekpo>-werks
                  lgort     = <lfs_ekpo>-lgort
                  username  = i_usuario
                  i_tipo_documento = i_tipo_documento
                TABLES
                  t_return  = lti_return
                  t_serno   = lti_serno     "Serie para el Material
                  t_charg   = lti_charg     "Lote para el Material
                  t_estatin = lti_estatin
                  t_matnr   = lti_matnr.

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
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
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
              ELSE.
                les_msg-num_doc = <lfs_ekpo>-ebeln.
                les_msg-msg = <lfs_return>-message.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ELSE.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'No Esta Pendiente por Picking'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
             ENDIF.
*}   INSERT
            ENDIF.
          ENDLOOP.

*.....Si el Parametro de Entrada Recibio un Número de Documento.
        ELSEIF i_documento_origen IS NOT INITIAL.
*.....Busco Registro Pendiente por Picking con Número de Documento
          READ TABLE lti_pick_eket WITH KEY ebeln = lv_ebeln " i_documento_origen
                                   ASSIGNING <lfs_eket>.
          IF sy-subrc EQ 0 .
*.....Busco Informacion de Cabecera del Pedido
            SELECT SINGLE ebeln bsart aedat lifnr
               FROM ekko
                  INTO les_bus_cab
                     WHERE ebeln EQ <lfs_eket>-ebeln. " AND
*                              bsart EQ <lfs_actividad>-cod_clase.
*.....Ajusto Nombre del Proveedor
            SELECT SINGLE lifnr name1
                 FROM lfa1
                   INTO les_comp_cab
                      WHERE lifnr EQ les_bus_cab-lifnr.
            IF sy-subrc EQ 0 .
              les_cab_ref-num_doc     = les_bus_cab-ebeln.
              les_cab_ref-fecha_crea  = les_bus_cab-aedat.
              les_cab_ref-tipo_doc    = les_bus_cab-bsart.
              les_cab_ref-cod_cliente = les_bus_cab-lifnr.
              les_cab_ref-nom_cliente = les_comp_cab-name1.
              APPEND les_cab_ref TO c_det_cabecera.
            ELSE.
              les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
              RETURN.
            ENDIF.
          ELSE.
            les_msg-num_doc = i_documento_origen.
            les_msg-msg = 'El Documento no Esta Pendiente por Picking'.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.
        ELSE.
          les_msg-msg = 'Debe Ingresar Información de Fechas y/o Número de Documento'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
      ELSE.
        les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
    ELSE.
      c_mensajes[] = mensajes[].
      RETURN.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_documento~get_detalle.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos para Importación.
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 20.06.2014    ER6K906839    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_eket,
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
   END OF ltp_eket.

*.....Typo para Busqueda Detalle Cabecera Documento EKKO
  TYPES: BEGIN OF ltp_bus_cab,
         ebeln TYPE ebeln,
         bsart TYPE esart,
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
          meins TYPE ze_uni_med,       "ekpo
          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          werks TYPE werks_d,          "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          loekz TYPE eloek,            "EKET Indicador de borrado en el documento de compras
          elikz TYPE elikz,            "EKET Indicador de entrega final
          retpo	TYPE retpo,            "Posición de devolución
          bsart TYPE esart,            "EKKO Clase de documento de compras
    END OF ltp_bus_pos.

  DATA :
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
           lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
           lti_pick_eket TYPE TABLE OF ltp_eket,
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
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
           lv_ebeln TYPE ebeln,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara  TYPE mara.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
                  <lfs_ekpo> TYPE ltp_bus_pos,
                  <lfs_eket> TYPE ltp_eket,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                  <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "IDEM
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido si Existe Información para el Usuario del Dispositivo
    IF detalles_usuario IS NOT INITIAL.


      MOVE i_documento_origen TO lv_ebeln.
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_ebeln
        IMPORTING
          output = lv_ebeln.
*.....Busqueda de Documentos Pendientes por Picking en tabla de Pos. Documentos de Compras
      SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz retpo
             FROM ekpo
              INTO CORRESPONDING FIELDS OF TABLE lti_pick_ekpo
                WHERE ebeln EQ lv_ebeln AND  "i_documento_origen AND
*                      werks EQ detalles_usuario-centro AND
*                      lgort EQ detalles_usuario-almacen AND
                      retpo	EQ ' ' and
                      LOEKZ EQ ' '.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0.
*.....Ordeno Tabla para un Mejor Procesamiento del For All Entries
        SORT  lti_pick_ekpo BY ebeln ebelp ASCENDING.

        SELECT ebeln ebelp menge wamng
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

*.....Verifico que Existan Documentos Pendientes por Picking
      IF lti_pick_eket IS NOT INITIAL.
*.....Ordeno Tabla por Número de Documento
        SORT lti_pick_eket BY ebeln ASCENDING.
*Ordeno la Tabla
        SORT lti_pick_ekpo BY ebeln  ASCENDING.
*.....Ajusto Tabla de Posiciones con su Clase de Documento Respectiva
        SELECT ebeln bsart
             FROM ekko
           INTO TABLE lti_ekko
                    FOR ALL ENTRIES IN lti_pick_ekpo
                            WHERE ebeln EQ lti_pick_ekpo-ebeln AND
                                  bsart EQ i_clase_documento.

*.....Busco el Codigo de la Clase para El documento Recibido
        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc EQ 0 .
          LOOP AT  lti_ekko ASSIGNING <lfs_ekko>  .
            DELETE lti_ekko WHERE bsart NE <lfs_actividad>-cod_clase.
          ENDLOOP.
        ELSE.
          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.

*.....Ajusto Información de la Tabla de Posiciones
        LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo>.
          READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.
          IF sy-subrc EQ 0 .
            <lfs_ekpo>-bsart =  <lfs_ekko>-bsart.
          ENDIF.
        ENDLOOP.

*.....Verifico que la Posición este Pendiente por Picking
        READ TABLE lti_pick_eket WITH KEY  ebeln = lv_ebeln "i_documento_origen
                                           ASSIGNING <lfs_eket>.
        IF sy-subrc EQ 0.
*-----------------------------------------------------------------------
*.......MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*       Modificado por: Andrés Felipe Castro.
          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .
            les_mara-matnr = <lfs_ekpo>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_mara = lti_mara.
*.......FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
          LOOP AT lti_pick_ekpo ASSIGNING <lfs_ekpo> WHERE ebeln = <lfs_eket>-ebeln .

            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_ekpo>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_ekpo>-werks
                  lgort     = <lfs_ekpo>-lgort
                  username  = i_usuario
                  i_tipo_documento = i_tipo_documento
                TABLES
                  t_return  = lti_return
                  t_serno   = lti_serno     "Serie para el Material
                  t_charg   = lti_charg     "Lote para el Material
                  t_estatin = lti_estatin
                  t_matnr   = lti_matnr.

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
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
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
              ELSE.
                les_msg-num_doc = <lfs_ekpo>-ebeln.
                les_msg-msg = <lfs_return>-message.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
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
  ENDIF.
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
*\
*\*...    Valido si es una ubicación principal o secundaria
*\         if les_ubicacion-ubic_default eq 'X'.
*\           les_ubicacion_suge-flag_ubic = 'D'.
*\         else.
*\           les_ubicacion_suge-flag_ubic = 'S'.
*\         endif.
*\ les_ubicacion_suge-cantidad  = les_ubicacion-cantidad.
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
*\   data: les_compras type ekko.
   data: les_compras type ZESD_CAB_CMP.
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
  data: les_lips_aux type lips.
*.... Estructura para el manejo del picking detalle
  data: les_pos_picking type ZEDSD_PICKING_DET.

*.... Estructuras para el manejo de las entregas
  data: lti_entregas type standard table of zesd_det_ent_pkg,
        les_entregas type zesd_det_ent_pkg.

*.... Copio el atributo de las entregas a la estructura
  move zif_lgtica_documento~t_entregas to lti_entregas.
  sort lti_entregas by entrega.
  delete adjacent duplicates from lti_entregas comparing  entrega.


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

                  "consulto posición superior si esta particionado por lotes.
          READ TABLE lti_lips into les_lips_aux with key matnr = les_lips-matnr VBELN = les_entregas-entrega
          POSNR = les_lips-UECHA.
          IF sy-subrc eq 0 .
            les_pos_picking-POSICION = les_lips_aux-posnr.
          else.
            les_pos_picking-POSICION = les_lips-POSNR.
          ENDIF.


          les_pos_picking-CANTIDAD = les_lips-LFIMG.
          les_pos_picking-UMC = les_lips-MEINS.
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
