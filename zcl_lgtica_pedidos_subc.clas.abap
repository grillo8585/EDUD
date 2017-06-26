class ZCL_LGTICA_PEDIDOS_SUBC definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_PEDIDOS_SUBC
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
  methods ZIF_LGTICA_DOCUMENTO~SET_ENTREGAS
    redefinition .
protected section.
*"* protected components of class ZCL_LGTICA_PEDIDOS_SUBC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_PEDIDOS_SUBC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_PEDIDOS_SUBC IMPLEMENTATION.


METHOD zif_lgtica_documento~generar_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Entregas para Proceso Pedidos de Subcontratacion
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 04.07.2014    ER6K906899    Marco Suarez        Creación
*-------------------------------------------------------------------------------*
*{   INSERT         ER3K900309                                        2
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
*}   INSERT
*.....Typo para Obtener Informacion del Plan de Entregas
  TYPES: BEGIN OF ltp_xeket ,
          ebeln TYPE eket-ebeln,
          ebelp TYPE eket-ebelp,
          etenr TYPE eket-etenr,
          eindt TYPE eket-eindt,
          menge TYPE eket-menge,
          wemng TYPE eket-wemng,
          rsnum TYPE eket-rsnum,
        END OF ltp_xeket.

*.....Typo para Obtener Informacion de la Necesidad
  TYPES: BEGIN OF ltp_xmdre ,
            rsnum TYPE  resb-rsnum,
            rspos TYPE  resb-rspos,
            matnr TYPE  resb-matnr,
            werks TYPE  resb-werks,
            lgort TYPE  resb-lgort,
            charg TYPE  resb-charg,
            bdter TYPE  resb-bdter,
            bdmng TYPE  resb-bdmng,
            meins TYPE meins,
            enmng TYPE  resb-enmng,
        END OF ltp_xmdre.

*.....Tipo para Dat.específ.expedición p.traslado a cl.doc.compras
  TYPES : BEGIN OF ltp_t161v,
          bstyp TYPE bstyp,
          bsart TYPE esart,
          reswk TYPE reswk,
          lblfa TYPE lblfa,
         END OF ltp_t161v.

*.....Typo para Centros/Sucursales
  TYPES : BEGIN OF ltp_t001w,
          werks TYPE werks_d,
          vtweg TYPE vtwiv,
          spart TYPE spaiv,
         END OF ltp_t001w.
*{   INSERT         ER3K900309                                        3
  DATA:
*.....Tabla Con msg de Errores
        lti_vbfs TYPE TABLE OF vbfs,
        les_vbfs TYPE ltp_vbfs,
        les_bapiret1 TYPE bapiret1.
DATA:
*.....Información de Cabecera del Documento Picking
        lti_picking_cab TYPE ZTTSD_PICKING,
*.....Información Detalles de Documento Picking
        lti_picking_det TYPE zttsd_picking_det,
*.....Información línea Detalles de Documento Picking
        les_picking_det like LINE OF lti_picking_det.
*}   INSERT

*.....Numero de Rango de Numeros
  DATA: l_e_nrrangenr TYPE inri-nrrangenr,
*.....Documento comercial cabecera proceso colectivo
        les_vbsk TYPE vbsk,
*.....Documentos Comerciales
        l_e_tvsa TYPE tvsa,
*.....Tabla de Entregas
        lti_lips TYPE TABLE OF lips,
*.....Tabla con Datos para Generar Entrega
        lti_komdlgn TYPE TABLE OF komdlgn,
        les_komdlgn TYPE komdlgn,
*.....Estructura de Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Estructura con Datos de Centro y Sucursales
        lti_t001w TYPE TABLE OF ltp_t001w,
*.....Estructura para Datos Cliente
        les_lfa1 TYPE lfa1,
*....Datos de Posicion Inicial
        lti_xeket TYPE  TABLE OF ltp_xeket,
*.....Datos de Posicion de Necesidad
        lti_xmdre TYPE TABLE OF ltp_xmdre,
*.....Tabla Interna para Dat.específ.expedición p.traslado a cl.doc.compras
        lti_t161v TYPE TABLE OF ltp_t161v,
*.....Estructuras de Entregas Generadas
        les_entregas TYPE zesd_detalles_entregas,
*.....Variable para Mensaje de Respuesta
        l_e_msg TYPE string,
*.....Varible Contador final de Posiciones para Resumen de Componentes
        l_e_pos TYPE n LENGTH 4,
*{   INSERT         ER3K900309                                        6
      e_syst type syst,
      number type SYMSGNO,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum,
*}   INSERT

        lo_pick TYPE REF TO zif_lgtica_pickable,
        lo_pick_clas TYPE REF TO zcl_lgtica_picking.

  FIELD-SYMBOLS: <lfs_ekpo> TYPE zesd_posiciones_compras,
*{   REPLACE        ER3K900309                                       10
*\                 <lfs_ekko> TYPE ekko,
                 <lfs_ekko> TYPE ZESD_CAB_CMP,
*}   REPLACE
                 <lfs_t161v> TYPE ltp_t161v,
                 <lfs_xmdre> TYPE ltp_xmdre,
                 <lfs_lips> TYPE lips,
                 <lfs_t001w> TYPE ltp_t001w,
                 <lfs_xeket> TYPE ltp_xeket.

*.....Metodo para Setiar Atributos
  CALL METHOD me->zif_lgtica_documento~set_compras
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Verifico que no Existan Errores en la Información Correspondiente al Usuario
  IF detalles_usuario IS NOT INITIAL.
*.....Llenado de Datos para Estructura les_vbsk
    les_vbsk-ernam = detalles_usuario-usuario.
    les_vbsk-erdat = sy-datum.
    les_vbsk-uzeit = sy-uzeit.
    les_vbsk-smart = 'L'.         "Tipo de Posicion L (Pedido de Subcontratacion)
*.....Documentos comerciales: Tipo de Posicion L
    SELECT SINGLE *
      FROM tvsa
         INTO l_e_tvsa
       WHERE smart = les_vbsk-smart.

    IF sy-subrc EQ 0.
      l_e_nrrangenr = l_e_tvsa-numki.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = l_e_nrrangenr
          object      = 'RV_SAMMG'
        IMPORTING
          number      = les_vbsk-sammg.
    ELSE.
*        les_msg-num_doc = <lfs_ekko>-ebeln.
      les_msg-msg = 'El rango de Números RV_SAMMG no Existe o no esta Parametrizado'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

*.....Los Atributos Tienen Informacion
    READ TABLE cabeceras_compras INDEX 1 ASSIGNING <lfs_ekko>.
*.....Datos Proveedor
    SELECT SINGLE *
        FROM lfa1
           INTO les_lfa1
              WHERE lifnr EQ <lfs_ekko>-lifnr.

    IF sy-subrc NE 0 .
*   les_msg-num_doc = <lfs_ekko>-ebeln.
      les_msg-msg = 'El Proveedor no Existe o no Esta Parametrizado'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.
*.....Llenado de Datos para Tabla lti_komdlgn
    SELECT  werks vtweg spart
        FROM t001w
           INTO TABLE lti_t001w.
    IF sy-subrc NE 0 .
*        les_msg-num_doc = <lfs_ekko>-ebeln.
      les_msg-msg = 'El Centro y/o Sucursal no Existe o no Esta Parametrizado'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

*.....Dat.específ.expedición p.traslado a cl.doc.compras
    SELECT bstyp bsart reswk lblfa
          FROM t161v
           INTO TABLE lti_t161v.
    IF sy-subrc NE 0 .
*        les_msg-num_doc = <lfs_ekko>-ebeln.
      les_msg-msg = 'Datos de Expedición Doc. Compras Invalidos'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

*.....Verificacion de las Posiciones del Documento de Compras, para la Subcontratacion
    LOOP AT detalles_compras ASSIGNING <lfs_ekpo> .
      IF <lfs_ekpo>-pstyp EQ '3' AND        "Tipo de posición del documento de compras
         <lfs_ekpo>-loekz NE 'L' AND        "Indicador de borrado en el documento de compras
         <lfs_ekpo>-elikz EQ space.         "Indicador de entrega final
        CONTINUE.
      ELSE.
        DELETE detalles_compras INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    IF detalles_compras[] IS INITIAL.
      les_msg-num_doc = <lfs_ekko>-ebeln.
      les_msg-msg = 'Error en Generación de Entrega, Verifique la Información'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

*.....Consulto Material en Posicion Pedido/Reparto (1)
    SELECT ebeln ebelp etenr eindt menge wemng rsnum
        FROM  eket
         INTO TABLE lti_xeket
            FOR ALL ENTRIES IN detalles_compras
            WHERE ebeln EQ detalles_compras-ebeln.

*.....Elimino Registro si la Cantidad de Reparto es Mayor a la Cantidad de Entrada de Mercancias
    LOOP AT lti_xeket ASSIGNING <lfs_xeket>.
      DELETE lti_xeket WHERE wemng GE <lfs_xeket>-menge.
    ENDLOOP.

    IF lti_xeket[] IS NOT INITIAL.
*{   INSERT         ER3K900309                                        8
*.... Obtengo los picknum de la sucontratación
        CALL METHOD zcl_lgtica_picking=>get_data_by_pedido
          EXPORTING
            i_pedido   = <lfs_ekko>-EBELN
          CHANGING
            C_PICKING_CAB_SUP = lti_picking_cab.

        IF lti_picking_cab is not INITIAL.
          select MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA
                 CONTEO CANTCONT UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO
            into table lti_picking_det
            FROM zmm_T_picking_dt
            FOR ALL ENTRIES IN  lti_picking_cab
            where PICKNUM eq lti_picking_cab-PICKNUM.
         ENDIF.
*}   INSERT
*.....Ordeno Tabla
      SORT lti_xeket BY ebeln ASCENDING.
*.....Consulto Reserva/Necesidades secundarias
      SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng
            FROM resb
            INTO TABLE lti_xmdre
                FOR ALL ENTRIES IN lti_xeket
                  WHERE rsnum EQ lti_xeket-rsnum
                       AND bdart EQ 'BB'        "Clase de necesidad
                       AND kzear EQ space       "Salida final de la reserva
                       AND dumps EQ space       "Indicador de posición dummy
                       AND shkzg EQ 'H'         "Indicador debe/haber
                       AND schgt EQ space       "Indicador: Material a granel
                       AND txtps EQ space       "Indicador de posición de texto
                       AND sobkz EQ space.      "Indicador de stock especial

*.....Verifico que Existan Registros (Resumen de Componentes, Posición Secundaria)
      IF lti_xmdre[] IS NOT INITIAL.
*.....Ordeno Tabla por Número de Material
        SORT lti_xmdre BY matnr ASCENDING.

        LOOP AT lti_xmdre ASSIGNING <lfs_xmdre>
                          WHERE werks EQ detalles_usuario-centro.
          l_e_pos = sy-tabix.
*.....Conversiones de Unidad de Medida
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input    = <lfs_xmdre>-meins
              language = 'S'
            IMPORTING
              output   = <lfs_xmdre>-meins.

*.....Lectura a Tabla de Centros y Sectores
          READ TABLE lti_t001w WITH KEY werks = <lfs_xmdre>-werks ASSIGNING <lfs_t001w>.

          les_komdlgn-vstel = <lfs_xmdre>-werks.   "Centro
          les_komdlgn-vkorg = <lfs_ekko>-ekorg.    "Organización de ventas "Organizacion de Compras
          les_komdlgn-vtweg = <lfs_t001w>-vtweg.   "Canal de distribución
          les_komdlgn-spart = <lfs_t001w>-spart.   "Sector
*.....Lectura a Tabla Dat.específ.expedición p.traslado a cl.doc.compras
          READ TABLE lti_t161v WITH KEY bstyp = space
                                   bsart = space
                                   reswk = <lfs_xmdre>-werks ASSIGNING <lfs_t161v>.
          IF  sy-subrc NE 0.
*        les_msg-num_doc = <lfs_ekko>-ebeln.
            les_msg-msg = 'Datos de Expedición Doc. Compras Invalidos'.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.

          les_komdlgn-lfart = <lfs_t161v>-lblfa."Clase de entrega
          les_komdlgn-kunwe = les_lfa1-kunnr.   "Destinatario de mercancías "Nº de cliente 1
          les_komdlgn-matnr = <lfs_xmdre>-matnr."Número de material
          les_komdlgn-wadat = <lfs_xmdre>-bdter."Fecha de salida de mercancías
          les_komdlgn-lfdat = <lfs_xmdre>-bdter."Fecha de entrega
*{   REPLACE        ER3K900309                                        9
*\          les_komdlgn-lfimg = <lfs_xmdre>-bdmng."Cantidad entregada efectivamente en UMV

          les_komdlgn-lfimg = <lfs_xmdre>-bdmng."Cantidad entregada efectivamente en UMV
          LOOP AT LTI_PICKING_DET into les_picking_det where material eq <lfs_xmdre>-matnr and POSICION eq <LFS_XMDRE>-RSPOS.
            les_komdlgn-lfimg = les_komdlgn-lfimg - les_picking_det-CANTCONT.
          ENDLOOP.
*}   REPLACE

          les_komdlgn-werks = <lfs_xmdre>-werks. "Centro
          les_komdlgn-vrkme = <lfs_xmdre>-meins. "Unidad de medida de venta "Unidad de medida base
          les_komdlgn-vgpos =  l_e_pos . "<lfs_xmdre>-rspos. "Número de posición de reserva/necesidades secundarias
          les_komdlgn-lgort = <lfs_xmdre>-lgort. "Almacen
          les_komdlgn-lifnr =  les_lfa1-lifnr.   "Número de cuenta del proveedor o acreedor
          APPEND les_komdlgn TO lti_komdlgn.
          CLEAR : les_komdlgn.
        ENDLOOP.
*.....Metodo para Generar Entrega
        CALL METHOD me->call_bapi_delivery2
          EXPORTING
            i_vbsk_i   = les_vbsk
            i_xkomdlgn = lti_komdlgn
*{   INSERT         ER3K900309                                        4
          IMPORTING
             e_syst            = e_syst
          CHANGING
            lti_vbfs =  lti_vbfs

*}   INSERT
          RECEIVING
            r_xlips    = lti_lips.

*.....Verifico que se Generaron Entregas
        IF lti_lips[] IS NOT INITIAL.
*.....Ordeno Tabla
          SORT lti_lips BY vbeln posnr ASCENDING.

*......Recorro Tabla de Entrega Generada
          LOOP AT lti_lips ASSIGNING <lfs_lips> .
            les_entregas-documento = <lfs_ekko>-ebeln.
            les_entregas-entrega =  <lfs_lips>-vbeln.
            APPEND les_entregas TO c_entregas.
            CLEAR : les_entregas.
          ENDLOOP.
*.....Elimino Registros Duplicados
          DELETE ADJACENT DUPLICATES FROM c_entregas COMPARING documento entrega.

          READ TABLE c_entregas INTO les_entregas INDEX 1.
*... Estruncturas para el manejo de la bapi Modificar Pedido de COmpra
          DATA:    return              TYPE STANDARD TABLE OF     bapiret2 ,
                   poheader            TYPE      bapimepoheader,
                   poheaderx           TYPE      bapimepoheaderx.
*... Llenar estructuras
          poheader-po_number = <lfs_ekko>-ebeln. "Orden de Compra
          poheader-collect_no = les_entregas-entrega. "Entrega Generada
          poheaderx-collect_no = 'X'.
*... Llamo Bapi
          CALL FUNCTION 'BAPI_PO_CHANGE'
            EXPORTING
              purchaseorder = poheader-po_number
              poheader      = poheader
              poheaderx     = poheaderx
            TABLES
              return        = return.
*Commit
*            me->entregas = c_entregas.
          me->zif_lgtica_documento~t_entregas = c_entregas.
          lo_pick = me.
*....
*{   REPLACE        ER3K900309                                        7
*\          CALL METHOD zcl_lgtica_picking=>create_by_pkble_ref
          CALL METHOD zcl_lgtica_picking=>CREATE_BY_PKBLE_REF_PCMP
*}   REPLACE
            EXPORTING
              i_ref_pickable = lo_pick.
*.....Actualizo Base de Datos
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

        ELSE.
*{   REPLACE        ER3K900309                                        5
*\          CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln
*\                       space  'Verifique Información' space INTO l_e_msg RESPECTING BLANKS.
*\          les_msg-num_doc = <lfs_ekko>-ebeln.
*\          les_msg-msg = l_e_msg.
*\          les_msg-type_msg = 'E'.
*\          APPEND les_msg TO c_mensajes.
*\          RETURN.
          IF lti_vbfs is not INITIAL.
             LOOP AT lti_vbfs INTO les_vbfs.
                    number  = les_vbfs-msgno.
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
                    CONCATENATE 'Entrega para el Doc ' space <lfs_ekko>-ebeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_ekko>-ebeln.
                    les_msg-msg = l_e_msg.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
                  ENDLOOP.
                  CLEAR : lti_vbfs.
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


          ENDIF.
*          CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_ekko>-ebeln
*                       space  'Verifique Información' space INTO l_e_msg RESPECTING BLANKS.
*          les_msg-num_doc = <lfs_ekko>-ebeln.
*          les_msg-msg = l_e_msg.
*          les_msg-type_msg = 'E'.
*          APPEND les_msg TO c_mensajes.
          RETURN.
*}   REPLACE
        ENDIF.
      ELSE.
        CONCATENATE 'No Existen Materiales para Tto de Componentes, Para el Doc.' space <lfs_ekko>-ebeln
                     space 'Verifique Información ' space INTO l_e_msg.
        les_msg-num_doc = <lfs_ekko>-ebeln.
        les_msg-msg = l_e_msg.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
    ELSE.
      les_msg-num_doc = <lfs_ekko>-ebeln.
*{   REPLACE        ER3K900309                                        1
*\      les_msg-msg = 'Error en Generación de Entrega, Verifique la Información'.
      les_msg-msg = 'Error en Generación de Entrega, Verifique la Información de Reparto en la Tabla EKET'.
*}   REPLACE
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
*********************************************************************************
* Definición de Constantes
*-------------------------------------------------------------------------------*
*CONSTANT: ………………….
*……………..
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_cod_mod TYPE c LENGTH 4 VALUE 'SUBC'.

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
          ebeln TYPE ekpo-ebeln, " Número del documento de compras
          ebelp TYPE ekpo-ebelp, " Número de posición del documento de compras
          matnr TYPE ekpo-matnr, " Número de material
          bukrs TYPE ekpo-bukrs, " Sociedad
          werks TYPE ekpo-werks, " Centro
          lgort TYPE ekpo-lgort, " Almacen
          menge TYPE ekpo-menge, " Cantidad de pedido
          meins TYPE ekpo-meins, " Unidad de medida de pedido
         END OF ltp_ti_ekpo.

*... tipo para la posición del documento de compras
*{   DELETE         ER3K900279                                        2
*\  TYPES: BEGIN OF ltp_ti_eket,
*\          ebeln TYPE eket-ebeln, " Número del documento de compras
*\          ebelp TYPE eket-ebelp, " Número de posición del documento de compras
*\          menge TYPE eket-menge, " Cantidad de reparto
*\          wamng TYPE eket-wamng, " Cantidad de salida
*\         END OF ltp_ti_eket.
*}   DELETE

*{   INSERT         ER3K900279                                        1
*.....Typo para Obtener Informacion de la Necesidad
  TYPES: BEGIN OF ltp_xmdre ,
            rsnum TYPE  resb-rsnum,
            rspos TYPE  resb-rspos,
            matnr TYPE  resb-matnr,
            werks TYPE  resb-werks,
            lgort TYPE  resb-lgort,
            charg TYPE  resb-charg,
            bdter TYPE  resb-bdter,
            bdmng TYPE  resb-bdmng,
            meins TYPE meins,
            enmng TYPE  resb-enmng,
        END OF ltp_xmdre.

*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_ti_eket,
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
           etenr TYPE eket-etenr,
          eindt TYPE eket-eindt,
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
          rsnum TYPE eket-rsnum,
   END OF ltp_ti_eket.

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
*.....Texto Material
  TYPES: BEGIN OF ltp_makt,
         matnr TYPE matnr,
         maktx TYPE maktx,
        END OF ltp_makt.

*.....Variable Estructura con detalles del Usuario
  DATA : les_det_usuario TYPE zmm_t_imp_etiq,
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
         lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
         lti_pick_eket TYPE TABLE OF ltp_ti_eket,
*.....Variable para Cantidad Pendiente por Picking
         l_e_pend TYPE etmen,
*.....Tabla para Ajustar Clase de Documento de Compras EKKO
*         lti_ekko TYPE TABLE OF ltp_bus_cab,
*.....Tabla Interna con Datos de Material
        lti_makt TYPE TABLE OF ltp_makt,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*.....Variable Estructura para Tabla de Mensajes
*         les_msg TYPE zesd_msg_picking,
*.....Datos de Posicion de Necesidad
        lti_xmdre TYPE TABLE OF ltp_xmdre,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
         l_e_desc_error TYPE string.
*Tabla interna de ubicación
  DATA: lti_ubicacion TYPE STANDARD TABLE OF zmm_t_ubicacion.
*... Tabla interna para consultar la tabla de ubicaciones
data: ti_zmm_t_ubicacion type standard table of zmm_t_ubicacion.
*...
data: les_ubicacion type  zmm_t_ubicacion.

data: les_ubicacion_suge type  ZESD_UBIC_SUG.


*... rangos para la clase de documento
*  DATA: rg_bsart TYPE RANGE OF ekko-bsart.
*  DATA: rl_bsart LIKE LINE OF rg_bsart.
* ...
*  DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
*  DATA: les_actv TYPE zmm_t_clase_pv.
*  DATA: lti_msgx TYPE STANDARD TABLE OF zesd_msg_picking.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
                  <lfs_ekpo> TYPE ltp_bus_pos,
                  <lfs_eket> TYPE ltp_ti_eket,
                  <lfs_xmdre> TYPE ltp_xmdre,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                   <lfs_makt> TYPE ltp_makt.
*... variable para la unidad de medida
  DATA: lv_meinh            TYPE string. " Unidad de medida tras conversión

*  DATA: l_o_ebeln TYPE ekko-ebeln.

*}   INSERT


*-------------------------------------------------------------------------------*
* Definición de Estructuras Tables
*-------------------------------------------------------------------------------*
*TABLES: t001.   "Códigos de Compañía

*-------------------------------------------------------------------------------*
* Definición de Estructuras
*-------------------------------------------------------------------------------*
*DATA:
  DATA: les_ekko TYPE ltp_ti_ekko.
  DATA: les_ekpo TYPE ltp_ti_ekpo.
  DATA: les_eket TYPE ltp_ti_eket.
  DATA: les_usuario TYPE zmm_t_imp_etiq.
  DATA: les_msg TYPE zesd_msg_picking.

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
  DATA: les_actv TYPE zmm_t_clase_pv.
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
        bsart IN  rg_bsart AND
        loekz EQ '' .  " Indicador de Borrado

        IF sy-subrc EQ 0.
*.... Adiciono el registro encontrado a la tabla interna de cabeceras
          APPEND les_ekko TO lti_ekko.

        ENDIF.
      ENDIF.
*.... Valido que es una consulta por fechas
    ELSEIF i_fec_ini_bus IS NOT INITIAL AND i_fec_fin_bus IS NOT INITIAL .

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
                     bsart EQ i_clase_documento AND
                     loekz EQ '' .  " Indicador de Borrado.

    ENDIF.
*....  Validamos que se haya encontrado registros de cabeceras
    IF lti_ekko[] IS NOT INITIAL.
*.... Se consultan las posisicones de cada documento
**.....Busqueda de Documentos Pendientes por Picking en tabla de Pos. Documentos de Compras
    SELECT ebeln ebelp matnr txz01 menge meins werks lgort loekz elikz
           FROM ekpo
            INTO CORRESPONDING FIELDS OF TABLE lti_ekpo
      for all entries in lti_ekko
              WHERE ebeln EQ lti_ekko-ebeln AND
          elikz EQ '' AND
        loekz EQ '' .
*      AND
*                    werks EQ detalles_usuario-centro AND
*                    lgort EQ detalles_usuario-almacen.
*... Se valida que se hayan encontrado posiciones
      IF sy-subrc EQ 0.
*... Se consultan el plan de repartos y sus cantidades para validar pendientes de picking
*{   REPLACE        ER3K900279                                        3
*\        SELECT ebeln ebelp menge wamng
        SELECT ebeln ebelp etenr eindt menge wamng rsnum
*}   REPLACE
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
              les_cab_ref-cod_cliente = les_ekko-reswk.

*.....Ajusto Nombre del Centro
              SELECT SINGLE name1
                   FROM t001w
                     INTO les_cab_ref-nom_cliente
                        WHERE werks EQ les_ekko-reswk.

*             les_cab_ref-nom_cliente= les_comp_cab-name1.
              APPEND les_cab_ref TO c_det_cabecera.
*{   INSERT         ER3K900279                                        4
              IF I_FLAG_DET EQ 'X'.
*.....Consulto Reserva/Necesidades secundarias
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng
              FROM resb
              INTO TABLE lti_xmdre
                  FOR ALL ENTRIES IN lti_eket
                    WHERE rsnum EQ lti_eket-rsnum

                         AND werks EQ detalles_usuario-centro
                         AND lgort EQ detalles_usuario-almacen
                         AND bdart EQ 'BB'        "Clase de necesidad
                         AND kzear EQ space       "Salida final de la reserva
                         AND dumps EQ space       "Indicador de posición dummy
                         AND shkzg EQ 'H'         "Indicador debe/haber
                         AND schgt EQ space       "Indicador: Material a granel
                         AND txtps EQ space       "Indicador de posición de texto
                         AND sobkz EQ space.      "Indicador de stock especial
*.....Verifico que Existan Registros
        IF sy-subrc EQ 0 AND lti_xmdre[] IS NOT INITIAL.
  IF lti_xmdre is not INITIAL.
*   Extraemos las ubicaciones, cantidades y unidades de medida del material
*   y lo llevamos a la tabla que se exportará
    SELECT material ubicacion UBIC_DEFAULT cantidad unidad_med
    INTO CORRESPONDING FIELDS OF TABLE lti_ubicacion
    FROM zmm_t_ubicacion
    FOR ALL ENTRIES IN lti_xmdre
    WHERE centro   EQ lti_xmdre-werks
      AND almacen  EQ lti_xmdre-lgort
      AND material EQ lti_xmdre-MATnr.


ENDIF.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_xmdre ASSIGNING <lfs_xmdre>.
            les_mara-matnr = <lfs_xmdre>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*{   REPLACE        ER3K900304                                        1
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

*.....Busqueda de Texto de Material
          SELECT matnr maktx
             FROM makt
                INTO TABLE lti_makt
                  FOR ALL ENTRIES IN lti_xmdre
                    WHERE matnr EQ lti_xmdre-matnr.
*{   INSERT         ER3K900304                                        2
            SORT lti_xmdre by matnr ASCENDING.
*}   INSERT

          LOOP AT lti_xmdre ASSIGNING <lfs_xmdre>.

*.....Limpieza de Variables
            CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
            l_e_matnr = <lfs_xmdre>-matnr.
            APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
            CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
              EXPORTING
                werk      = <lfs_xmdre>-werks
                lgort     = <lfs_xmdre>-lgort
                username  = i_usuario
              TABLES
                t_return  = lti_return
                t_serno   = lti_serno     "Serie para el Material
                t_charg   = lti_charg     "Lote para el Material
                t_estatin = lti_estatin
                t_matnr   = lti_matnr.

*.....Verifico que no Existan Errores
            READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
*{   REPLACE        ER3K900304                                        3
*\            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*\*.....Verifico que el Material Tenga Número de Serie
*\              IF lti_serno IS NOT INITIAL.
*\                APPEND LINES OF lti_serno TO c_seriales.
*\              ENDIF.
*\
*\*.....Verifico si el Material Tiene Número de Lote
*\              IF lti_charg  IS NOT INITIAL  .
*\                APPEND LINES OF lti_charg TO c_lotes.
*\              ENDIF.
*\
*\*.....Limpieza de Variables
*\              CLEAR : lti_detalle, lti_mensajes.
*\*.....Función para Obtener EANs de un Material
*\              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                EXPORTING
*\                  matnr      = <lfs_xmdre>-matnr
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
*\                    les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
*\                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
*\                    les_det_pos-material = <lfs_xmdre>-matnr.
*\*.....Lectura a Tabla de Texto de Material
*\                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
*\                    IF  sy-subrc EQ 0 .
*\                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
*\                    ENDIF.
*\                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
*\                    les_det_pos-ubic_fija = ' '.
*\                    les_det_pos-ubic_tmp =  ' '.
*\                    les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\*       Realizar la conversión de la unidad de medida
*\                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                      EXPORTING
*\                        input          = <lfs_xmdre>-meins
*\                        language       = 'S'
*\                      IMPORTING
*\                        output         = lv_meinh
*\                      EXCEPTIONS
*\                        unit_not_found = 1
*\                        OTHERS         = 2.
*\                    IF lv_meinh EQ '004'.
*\                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
*\                    ELSE.
*\                      les_det_pos-uni_med_doc = lv_meinh.
*\                    ENDIF.
*\*.....Verifico que el Material Tenga Número de Serie
*\                    IF lti_serno IS NOT INITIAL.
*\*                  APPEND LINES OF lti_serno TO c_seriales.
*\                      les_det_pos-seriales = 'X'.
*\                    ELSE.
*\                      les_det_pos-seriales = ' '.
*\                    ENDIF.
*\*                ENDIF.
*\*.....Verifico si el Material Tiene Número de Lote
*\                    IF lti_charg  IS NOT INITIAL  .
*\*                  APPEND LINES OF lti_charg TO c_lotes.
*\                      les_det_pos-numero_lote = 'X'.
*\                    ELSE.
*\                      les_det_pos-numero_lote = ' '.
*\                    ENDIF.
*\*.....Datos de Detalle EAN
*\                    les_det_pos-ean = <lfs_detalle>-ean.
*\                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                    APPEND les_det_pos TO c_det_posiciones.
*\                    CLEAR les_det_pos.
*\                  ENDLOOP.
*\                ELSE.
*\                  les_msg-num_doc = ' '.
*\                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
*\                  INTO l_e_desc_error SEPARATED BY space.
*\                  les_msg-msg = l_e_desc_error.
*\                  les_msg-type_msg = 'E'.
*\                  APPEND les_msg TO c_mensajes.
*\                ENDIF.
*\              ENDIF.
*\            ELSE.
*\              les_msg-num_doc = <lfs_ekpo>-ebeln.
*\              les_msg-msg = <lfs_return>-message.
*\              les_msg-type_msg = 'E'.
*\              APPEND les_msg TO c_mensajes.
*\            ENDIF.
            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico que el Material Tenga Número de Serie
              IF lti_serno IS NOT INITIAL.
                APPEND LINES OF lti_serno TO c_seriales.
              ENDIF.

*.....Verifico si el Material Tiene Número de Lote
              IF lti_charg  IS NOT INITIAL  .
                APPEND LINES OF lti_charg TO c_lotes.
              ENDIF.
            ENDIF.

            IF I_FLAG_AGRUP is not INITIAL.
              CLEAR les_det_pos.
*              les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
                  les_det_pos-num_entrega = les_cab_ref-num_doc.
*                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
                    les_det_pos-material = <lfs_xmdre>-matnr.
*.....Lectura a Tabla de Texto de Material
                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
                    IF  sy-subrc EQ 0 .
                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
                    ENDIF.
                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
                                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X'.
                      IF sy-subrc eq 0.
                        les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                        ELSE.
                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material.
                          IF sy-subrc eq 0.
                            les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                          ENDIF.
                      ENDIF.

*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_xmdre>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
*.....Verifico que el Material Tenga Número de Serie
                    IF lti_serno IS NOT INITIAL.
*                  APPEND LINES OF lti_serno TO c_seriales.
                      les_det_pos-seriales = 'X'.
                    ELSE.
                      les_det_pos-seriales = ' '.
                    ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                    IF lti_charg  IS NOT INITIAL  .
*                  APPEND LINES OF lti_charg TO c_lotes.
                      les_det_pos-numero_lote = 'X'.
                    ELSE.
                      les_det_pos-numero_lote = ' '.
                    ENDIF.
*.....Datos de Detalle EAN
                    READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_xmdre>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.

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
                  matnr      = <lfs_xmdre>-matnr
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
                    les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
                    les_det_pos-material = <lfs_xmdre>-matnr.
*.....Lectura a Tabla de Texto de Material
                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
                    IF  sy-subrc EQ 0 .
                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
                    ENDIF.
                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
                                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X'.
                      IF sy-subrc eq 0.
                        les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                        ELSE.
                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material.
                          IF sy-subrc eq 0.
                            les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                          ENDIF.
                      ENDIF.

*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_xmdre>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
*.....Verifico que el Material Tenga Número de Serie
                    IF lti_serno IS NOT INITIAL.
*                  APPEND LINES OF lti_serno TO c_seriales.
                      les_det_pos-seriales = 'X'.
                    ELSE.
                      les_det_pos-seriales = ' '.
                    ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                    IF lti_charg  IS NOT INITIAL  .
*                  APPEND LINES OF lti_charg TO c_lotes.
                      les_det_pos-numero_lote = 'X'.
                    ELSE.
                      les_det_pos-numero_lote = ' '.
                    ENDIF.
*.....Datos de Detalle EAN
                    les_det_pos-ean = <lfs_detalle>-ean.
                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                    APPEND les_det_pos TO c_det_posiciones.
                    CLEAR les_det_pos.
                  ENDLOOP.
                ELSE.
                  les_msg-num_doc = ' '.
                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
                  INTO l_e_desc_error SEPARATED BY space.
                  les_msg-msg = l_e_desc_error.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDIF.
              ENDIF.
*            ELSE.
*              les_msg-num_doc = <lfs_ekpo>-ebeln.
*              les_msg-msg = <lfs_return>-message.
*              les_msg-type_msg = 'E'.
*              APPEND les_msg TO c_mensajes.
*            ENDIF.
*}   REPLACE


          ENDLOOP.
        ENDIF.
              ENDIF.
*}   INSERT
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
* Descripción  : Detalle de Documentos para Pedidos de subcontratación, para el Codigo
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
  CONSTANTS: lc_proceso TYPE c LENGTH 20 VALUE 'PICKING'.
  CONSTANTS: lc_cod_mod TYPE c LENGTH 4 VALUE 'SUBC'.
*.....Typo para Obtener Informacion de la Necesidad
  TYPES: BEGIN OF ltp_xmdre ,
            rsnum TYPE  resb-rsnum,
            rspos TYPE  resb-rspos,
            matnr TYPE  resb-matnr,
            werks TYPE  resb-werks,
            lgort TYPE  resb-lgort,
            charg TYPE  resb-charg,
            bdter TYPE  resb-bdter,
            bdmng TYPE  resb-bdmng,
            meins TYPE meins,
            enmng TYPE  resb-enmng,
        END OF ltp_xmdre.

*.....Typo para Busqueda de Documento Pendiente por Picking (EKET)
  TYPES : BEGIN OF ltp_eket,
          ebeln TYPE ebeln,      "Número del documento de compras
          ebelp TYPE ebelp,      "Número de posición del documento de compras
           etenr TYPE eket-etenr,
          eindt TYPE eket-eindt,
          menge TYPE etmen,      "Cantidad de reparto
          wamng TYPE wamng,      "Cantidad de salida
          rsnum TYPE eket-rsnum,
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
*.....Texto Material
  TYPES: BEGIN OF ltp_makt,
         matnr TYPE matnr,
         maktx TYPE maktx,
        END OF ltp_makt.

*.....Variable Estructura con detalles del Usuario
  DATA : les_det_usuario TYPE zmm_t_imp_etiq,
*.....Tabla para Busqueda de Documentos Pendiente por Picking EKPO
         lti_pick_ekpo TYPE TABLE OF ltp_bus_pos,
*.....Tabla para Busqueda de Documntos Pendientes por Picking EKET
         lti_pick_eket TYPE TABLE OF ltp_eket,
*.....Variable para Cantidad Pendiente por Picking
         l_e_pend TYPE etmen,
*.....Tabla para Ajustar Clase de Documento de Compras EKKO
         lti_ekko TYPE TABLE OF ltp_bus_cab,
*.....Tabla Interna con Datos de Material
        lti_makt TYPE TABLE OF ltp_makt,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*.....Variable Estructura para Tabla de Mensajes
         les_msg TYPE zesd_msg_picking,
*.....Datos de Posicion de Necesidad
        lti_xmdre TYPE TABLE OF ltp_xmdre,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
         l_e_desc_error TYPE string.

*... rangos para la clase de documento
  DATA: rg_bsart TYPE RANGE OF ekko-bsart.
  DATA: rl_bsart LIKE LINE OF rg_bsart.
* ...
  DATA: lti_actv TYPE STANDARD TABLE OF zmm_t_clase_pv.
  DATA: les_actv TYPE zmm_t_clase_pv.
  DATA: lti_msgx TYPE STANDARD TABLE OF zesd_msg_picking.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_ekko> TYPE ltp_bus_cab,
                  <lfs_ekpo> TYPE ltp_bus_pos,
                  <lfs_eket> TYPE ltp_eket,
                  <lfs_xmdre> TYPE ltp_xmdre,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                   <lfs_makt> TYPE ltp_makt.
*... variable para la unidad de medida
  DATA: lv_meinh            TYPE string. " Unidad de medida tras conversión

  DATA: l_o_ebeln TYPE ekko-ebeln.
*.....Valido si Existe Información para el Usuario del Dispositivo
  IF detalles_usuario IS NOT INITIAL.

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
        loekz EQ '' .
*      AND
*                    werks EQ detalles_usuario-centro AND
*                    lgort EQ detalles_usuario-almacen.
*.....Verifico que Existan Registros
    IF sy-subrc EQ 0.
*.....Ordeno Tabla para un Mejor Proceso del For All Entries
      SORT lti_pick_ekpo BY ebeln ebelp ASCENDING.

      SELECT ebeln ebelp etenr eindt menge wamng rsnum
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
*.....Consulto Reserva/Necesidades secundarias
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng
              FROM resb
              INTO TABLE lti_xmdre
                  FOR ALL ENTRIES IN lti_pick_eket
                    WHERE rsnum EQ lti_pick_eket-rsnum

                         AND werks EQ detalles_usuario-centro
                         AND lgort EQ detalles_usuario-almacen
                         AND bdart EQ 'BB'        "Clase de necesidad
                         AND kzear EQ space       "Salida final de la reserva
                         AND dumps EQ space       "Indicador de posición dummy
                         AND shkzg EQ 'H'         "Indicador debe/haber
                         AND schgt EQ space       "Indicador: Material a granel
                         AND txtps EQ space       "Indicador de posición de texto
                         AND sobkz EQ space.      "Indicador de stock especial
*.....Verifico que Existan Registros
        IF sy-subrc EQ 0 AND lti_xmdre[] IS NOT INITIAL.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_xmdre ASSIGNING <lfs_xmdre>.
            les_mara-matnr = <lfs_xmdre>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*{   REPLACE        ER3K900304                                        1
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

*.....Busqueda de Texto de Material
          SELECT matnr maktx
             FROM makt
                INTO TABLE lti_makt
                  FOR ALL ENTRIES IN lti_xmdre
                    WHERE matnr EQ lti_xmdre-matnr.
*{   INSERT         ER3K900304                                        2
            SORT lti_xmdre by matnr ASCENDING.
*}   INSERT

          LOOP AT lti_xmdre ASSIGNING <lfs_xmdre>.

*.....Limpieza de Variables
            CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
            l_e_matnr = <lfs_xmdre>-matnr.
            APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
            CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
              EXPORTING
                werk      = <lfs_xmdre>-werks
                lgort     = <lfs_xmdre>-lgort
                username  = i_usuario
              TABLES
                t_return  = lti_return
                t_serno   = lti_serno     "Serie para el Material
                t_charg   = lti_charg     "Lote para el Material
                t_estatin = lti_estatin
                t_matnr   = lti_matnr.

*.....Verifico que no Existan Errores
            READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
*{   REPLACE        ER3K900304                                        3
*\            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*\*.....Verifico que el Material Tenga Número de Serie
*\              IF lti_serno IS NOT INITIAL.
*\                APPEND LINES OF lti_serno TO c_seriales.
*\              ENDIF.
*\
*\*.....Verifico si el Material Tiene Número de Lote
*\              IF lti_charg  IS NOT INITIAL  .
*\                APPEND LINES OF lti_charg TO c_lotes.
*\              ENDIF.
*\
*\*.....Limpieza de Variables
*\              CLEAR : lti_detalle, lti_mensajes.
*\*.....Función para Obtener EANs de un Material
*\              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                EXPORTING
*\                  matnr      = <lfs_xmdre>-matnr
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
*\                    les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
*\                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
*\                    les_det_pos-material = <lfs_xmdre>-matnr.
*\*.....Lectura a Tabla de Texto de Material
*\                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
*\                    IF  sy-subrc EQ 0 .
*\                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
*\                    ENDIF.
*\                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
*\                    les_det_pos-ubic_fija = ' '.
*\                    les_det_pos-ubic_tmp =  ' '.
*\                    les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\*       Realizar la conversión de la unidad de medida
*\                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                      EXPORTING
*\                        input          = <lfs_xmdre>-meins
*\                        language       = 'S'
*\                      IMPORTING
*\                        output         = lv_meinh
*\                      EXCEPTIONS
*\                        unit_not_found = 1
*\                        OTHERS         = 2.
*\                    IF lv_meinh EQ '004'.
*\                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
*\                    ELSE.
*\                      les_det_pos-uni_med_doc = lv_meinh.
*\                    ENDIF.
*\*.....Verifico que el Material Tenga Número de Serie
*\                    IF lti_serno IS NOT INITIAL.
*\*                  APPEND LINES OF lti_serno TO c_seriales.
*\                      les_det_pos-seriales = 'X'.
*\                    ELSE.
*\                      les_det_pos-seriales = ' '.
*\                    ENDIF.
*\*                ENDIF.
*\*.....Verifico si el Material Tiene Número de Lote
*\                    IF lti_charg  IS NOT INITIAL  .
*\*                  APPEND LINES OF lti_charg TO c_lotes.
*\                      les_det_pos-numero_lote = 'X'.
*\                    ELSE.
*\                      les_det_pos-numero_lote = ' '.
*\                    ENDIF.
*\*.....Datos de Detalle EAN
*\                    les_det_pos-ean = <lfs_detalle>-ean.
*\                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                    APPEND les_det_pos TO c_det_posiciones.
*\                    CLEAR les_det_pos.
*\                  ENDLOOP.
*\                ELSE.
*\                  les_msg-num_doc = ' '.
*\                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
*\                  INTO l_e_desc_error SEPARATED BY space.
*\                  les_msg-msg = l_e_desc_error.
*\                  les_msg-type_msg = 'E'.
*\                  APPEND les_msg TO c_mensajes.
*\                ENDIF.
*\              ENDIF.
*\            ELSE.
*\              les_msg-num_doc = <lfs_ekpo>-ebeln.
*\              les_msg-msg = <lfs_return>-message.
*\              les_msg-type_msg = 'E'.
*\              APPEND les_msg TO c_mensajes.
*\            ENDIF.
            IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico que el Material Tenga Número de Serie
              IF lti_serno IS NOT INITIAL.
                APPEND LINES OF lti_serno TO c_seriales.
              ENDIF.

*.....Verifico si el Material Tiene Número de Lote
              IF lti_charg  IS NOT INITIAL  .
                APPEND LINES OF lti_charg TO c_lotes.
              ENDIF.
            ENDIF.

            IF I_FLAG_AGRUP is not INITIAL.
              CLEAR les_det_pos.
              les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
*                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
                    les_det_pos-material = <lfs_xmdre>-matnr.
*.....Lectura a Tabla de Texto de Material
                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
                    IF  sy-subrc EQ 0 .
                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
                    ENDIF.
                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_xmdre>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
*.....Verifico que el Material Tenga Número de Serie
                    IF lti_serno IS NOT INITIAL.
*                  APPEND LINES OF lti_serno TO c_seriales.
                      les_det_pos-seriales = 'X'.
                    ELSE.
                      les_det_pos-seriales = ' '.
                    ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                    IF lti_charg  IS NOT INITIAL  .
*                  APPEND LINES OF lti_charg TO c_lotes.
                      les_det_pos-numero_lote = 'X'.
                    ELSE.
                      les_det_pos-numero_lote = ' '.
                    ENDIF.
*.....Datos de Detalle EAN
                    READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_xmdre>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.

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
                  matnr      = <lfs_xmdre>-matnr
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
                    les_det_pos-num_entrega = <lfs_xmdre>-rsnum.
                    les_det_pos-pos_entrega = <lfs_xmdre>-rspos.
                    les_det_pos-material = <lfs_xmdre>-matnr.
*.....Lectura a Tabla de Texto de Material
                    READ TABLE lti_makt WITH  KEY matnr = <lfs_xmdre>-matnr ASSIGNING <lfs_makt>.
                    IF  sy-subrc EQ 0 .
                      les_det_pos-desc_material = <lfs_makt>-maktx.  "Texto breve de material
                    ENDIF.
                    les_det_pos-cant_por_pick = <lfs_xmdre>-bdmng.
                    les_det_pos-ubic_fija = ' '.
                    les_det_pos-ubic_tmp =  ' '.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = <lfs_xmdre>-meins
                        language       = 'S'
                      IMPORTING
                        output         = lv_meinh
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF lv_meinh EQ '004'.
                      les_det_pos-uni_med_doc = <lfs_xmdre>-meins.
                    ELSE.
                      les_det_pos-uni_med_doc = lv_meinh.
                    ENDIF.
*.....Verifico que el Material Tenga Número de Serie
                    IF lti_serno IS NOT INITIAL.
*                  APPEND LINES OF lti_serno TO c_seriales.
                      les_det_pos-seriales = 'X'.
                    ELSE.
                      les_det_pos-seriales = ' '.
                    ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                    IF lti_charg  IS NOT INITIAL  .
*                  APPEND LINES OF lti_charg TO c_lotes.
                      les_det_pos-numero_lote = 'X'.
                    ELSE.
                      les_det_pos-numero_lote = ' '.
                    ENDIF.
*.....Datos de Detalle EAN
                    les_det_pos-ean = <lfs_detalle>-ean.
                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                    APPEND les_det_pos TO c_det_posiciones.
                    CLEAR les_det_pos.
                  ENDLOOP.
                ELSE.
                  les_msg-num_doc = ' '.
                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
                  INTO l_e_desc_error SEPARATED BY space.
                  les_msg-msg = l_e_desc_error.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDIF.
              ENDIF.
*            ELSE.
*              les_msg-num_doc = <lfs_ekpo>-ebeln.
*              les_msg-msg = <lfs_return>-message.
*              les_msg-type_msg = 'E'.
*              APPEND les_msg TO c_mensajes.
*            ENDIF.
*}   REPLACE


          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.
************************CAMBIO*********************************************************************************
***.....Verifico que Existan Documentos Pendientes por Picking
**    IF lti_pick_eket IS NOT INITIAL.
***.....Ordeno Tabla por Número de Documento
**      SORT lti_pick_eket BY ebeln ASCENDING.
***... se consultan las clases de documento asociadas a el tipo de documeto o activadad
**      CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
**        EXPORTING
**          proceso         = 'PICKING'
**          cod_modulo      = lc_cod_mod
**        TABLES
**          tipos_actividad = lti_actv
**          t_mensajes      = lti_msgx.
***... se validan que se hayan encontrado registros
**      IF lti_actv[] IS NOT INITIAL.
***... se recorre la tabla interna y se llenan los rangos
**        LOOP AT lti_actv INTO les_actv.
**          rl_bsart-sign = 'I'.
**          rl_bsart-option = 'EQ'.
**          rl_bsart-low = les_actv-cod_clase.
**          APPEND rl_bsart TO  rg_bsart.
**        ENDLOOP.
***.....Busco Clase de Documento de Compras para Documentos Pendientes por Picking
**        SELECT ebeln bsart
**           FROM  ekko
**             INTO TABLE lti_ekko
**              FOR ALL ENTRIES IN lti_pick_eket
**                WHERE ebeln EQ lti_pick_eket-ebeln
**                 AND bsart IN rg_bsart.
***.....Verifico que Existan Registros
**        IF sy-subrc EQ 0.
**          LOOP AT lti_pick_ekpo ASSIGNING  <lfs_ekpo>.
***.....Verifico que la Posición este Pendiente por Picking
**            READ TABLE lti_pick_eket WITH KEY  ebeln = <lfs_ekpo>-ebeln
**                                               ebelp = <lfs_ekpo>-ebelp TRANSPORTING NO FIELDS.
**
**            IF sy-subrc EQ 0.
***.....Ajusto a la Posición su Clase de Documento
**              READ TABLE lti_ekko WITH KEY ebeln = <lfs_ekpo>-ebeln ASSIGNING <lfs_ekko>.
**              IF sy-subrc EQ 0.
**                <lfs_ekpo>-bsart = <lfs_ekko>-bsart.
***.....Limpieza de Variables
**              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
***.....Inserto Registro con Número de Material
**              l_e_matnr = <lfs_ekpo>-matnr.
**              APPEND l_e_matnr TO lti_matnr.
***.....Función para Obtener el la Serie y el Número de Lote para un Material
**              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
**                EXPORTING
**                  werk      = <lfs_ekpo>-werks
**                  lgort     = <lfs_ekpo>-lgort
**                  username  = i_usuario
**                TABLES
**                  t_return  = lti_return
**                  t_serno   = lti_serno     "Serie para el Material
**                  t_charg   = lti_charg     "Lote para el Material
**                  t_estatin = lti_estatin
**                  t_matnr   = lti_matnr.
**
***.....Verifico que no Existan Errores
**              READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
**              IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
***.....Verifico que el Material Tenga Número de Serie
**                IF lti_serno IS NOT INITIAL.
**                  APPEND LINES OF lti_serno TO c_seriales.
**                  <lfs_ekpo>-seriales = 'X'.
**                ELSE.
**                  <lfs_ekpo>-seriales = ' '.
**                ENDIF.
***                ENDIF.
***.....Verifico si el Material Tiene Número de Lote
**                IF lti_charg  IS NOT INITIAL  .
**                  APPEND LINES OF lti_charg TO c_lotes.
**                  <lfs_ekpo>-numero_lote = 'X'.
**                ELSE.
**                  <lfs_ekpo>-numero_lote = ' '.
**                ENDIF.
***                ENDIF.
***.....Limpieza de Variables
**                CLEAR : lti_detalle, lti_mensajes.
***.....Función para Obtener EANs de un Material
**                  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
**                    EXPORTING
**                      matnr      = <lfs_ekpo>-matnr
**                    TABLES
**                      t_detalle  = lti_detalle
**                      t_mensajes = lti_mensajes.
**
***.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
**                  READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
**                  IF sy-subrc EQ 0 .
**                    les_msg-num_doc = <lfs_mensajes>-num_doc.
**                    les_msg-msg = <lfs_mensajes>-msg.
**                    les_msg-type_msg = <lfs_mensajes>-msg.
**                    APPEND les_msg TO c_mensajes.
**                  ELSE.
***.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
**                    IF lti_detalle IS NOT INITIAL  .
**                      LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
**                        les_det_pos-num_entrega = <lfs_ekpo>-ebeln.
**                        les_det_pos-pos_entrega = <lfs_ekpo>-ebelp.
**                        les_det_pos-material = <lfs_ekpo>-matnr.
**                        les_det_pos-desc_material = <lfs_ekpo>-txz01.
**                        les_det_pos-cant_por_pick = <lfs_ekpo>-menge.
**                        les_det_pos-ubic_fija = ' '.
**                        les_det_pos-ubic_tmp =  ' '.
**                        les_det_pos-cant_contada =  ' '.
***                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
***       Realizar la conversión de la unidad de medida
**                        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
**                          EXPORTING
**                            input          = <lfs_ekpo>-meins
**                            language       = 'S'
**                          IMPORTING
**                            output         = lv_meinh
**                          EXCEPTIONS
**                            unit_not_found = 1
**                            OTHERS         = 2.
**                        IF lv_meinh EQ '004'.
**                          les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
**                        ELSE.
**                          les_det_pos-uni_med_doc = lv_meinh.
**                        ENDIF.
**
**                        les_det_pos-seriales = <lfs_ekpo>-seriales.
**                        les_det_pos-numero_lote = <lfs_ekpo>-numero_lote.
***.....Datos de Detalle EAN
**                        les_det_pos-ean = <lfs_detalle>-ean.
**                        les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
**                        les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
**                        APPEND les_det_pos TO c_det_posiciones.
**                        CLEAR les_det_pos.
**                      ENDLOOP.
**                    ELSE.
**                      les_msg-num_doc = ' '.
**                      CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_ekpo>-matnr
**                      INTO l_e_desc_error SEPARATED BY space.
**                      les_msg-msg = l_e_desc_error.
**                      les_msg-type_msg = 'E'.
**                      APPEND les_msg TO c_mensajes.
**                    ENDIF.
**                  ENDIF.
**                ELSE.
**                les_msg-num_doc = <lfs_ekpo>-ebeln.
**                les_msg-msg = <lfs_return>-message.
**                les_msg-type_msg = 'E'.
**                APPEND les_msg TO c_mensajes.
**                ENDIF.
**              ENDIF.
**            ELSE.
**              CONTINUE.
**            ENDIF.
**          ENDLOOP.
**        ENDIF.
**      ENDIF.
**    ELSE.
**      les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
**      les_msg-type_msg = 'E'.
**      APPEND les_msg TO c_mensajes.
**      RETURN.
**    ENDIF.
************************CAMBIO*********************************************************************************
  ELSE.
    c_mensajes[] = mensajes[].
    RETURN.
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


METHOD zif_lgtica_documento~load_document.
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
  CALL METHOD me->zif_lgtica_documento~set_compras
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Metodo para Setiar Atributos de entregas
  CALL METHOD me->zif_lgtica_documento~set_entregas
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Indicador para Proceso que Genera Entrega
  c_indicador = 'X'.
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
*... tipo para la cabecera del documento de compras
  TYPES:BEGIN OF ltp_ti_ekko,
          ebeln TYPE ekko-ebeln, " Número del documento de compras
          bukrs TYPE ekko-bukrs, " Sociedad
          bsart TYPE ekko-bsart, " Clase de documento de compras
          lifnr TYPE ekko-lifnr, " Número de cuenta del proveedor
          reswk TYPE ekko-reswk, " Centro suministrador en el pedido de transporte
          aedat TYPE ekko-aedat, " fecha de creación
          submi TYPE ekko-submi, "
         END OF ltp_ti_ekko.

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
*.....Estructura con Datos Principales de Orden de compra
          les_ekko TYPE ltp_ti_ekko,
          lti_ekko TYPE STANDARD TABLE OF ltp_ti_ekko,
*.... Estructura para llenar la información de retorno de la entrega
          les_entrega TYPE zesd_det_ent_pkg,
*.... Variable para almacenar el número de documento
          v_vgbel TYPE vgbel,
          v_ebeln TYPE ebeln,
*.....Estructura para Cabecera Ventas
*{   REPLACE        ER3K900309                                        1
*\          les_cab_ventas TYPE vbak,
          les_cab_ventas TYPE ZESD_CAB_PED,
*}   REPLACE
*{   REPLACE        ER3K900309                                        2
*\          les_cab_compras TYPE ekko,
          les_cab_compras TYPE ZESD_CAB_CMP,
*}   REPLACE

*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF vgbel,
          ls_numero LIKE LINE OF rl_numero.

*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .
    MOVE i_documento_origen TO v_vgbel.
    MOVE i_documento_origen TO v_ebeln.
*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_vgbel
      IMPORTING
        output = v_vgbel.

*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_ebeln
      IMPORTING
        output = v_ebeln.

*.....Creo Rango de Documento

    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = v_vgbel.
    APPEND ls_numero TO rl_numero.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lti_ekko
    FROM ekko
    WHERE ebeln  EQ v_ebeln.

  IF lti_ekko IS NOT INITIAL.


*.... Se toman las entregas asociadas al Número de documento ingresado
    SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
    INTO TABLE lti_lips
      FROM lips
      FOR ALL ENTRIES IN lti_ekko
      WHERE vbeln EQ lti_ekko-submi.

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
  ENDIF.
ENDMETHOD.


method ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING.
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
  sort lti_entregas by entrega.
  DELETE ADJACENT DUPLICATES FROM lti_entregas comparing entrega.
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
endmethod.


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

"consulto posición superior si esta particionado por lotes.
          READ TABLE lti_lips into les_lips_aux with key matnr = les_lips-matnr VBELN = les_entregas-entrega
          POSNR = les_lips-UECHA.
          IF sy-subrc eq 0 .
            les_pos_picking-POSICION = les_lips_aux-posnr.
          else.
            les_pos_picking-POSICION = les_lips-POSNR.
          ENDIF.
          les_pos_picking-MATERIAL = les_lips-matnr.
*          les_pos_picking-POSICION = les_lips-POSNR.
*{   REPLACE        ER3K900309                                        1
*\          les_pos_picking-CANTIDAD = les_lips-LFIMG.
*\          les_pos_picking-UMC = les_lips-MEINS.
          les_pos_picking-CANTIDAD = les_lips-LGMNG.

          les_pos_picking-UMC = les_lips-MEINS.
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
