class ZCL_LGTICA_RESERVA_INV definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_RESERVA_INV
*"* do not include other source files here!!!
public section.

  methods ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA_PACKING
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE_PACKING
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT
    redefinition .
  methods ZIF_LGTICA_PACKING~GET_CABECERA_PACKING
    redefinition .
  methods ZIF_LGTICA_PACKING~GET_DETALLE_PACKING
    redefinition .
  methods ZIF_LGTICA_PACKING~GET_HEADER_PACKING
    redefinition .
  methods ZIF_LGTICA_PACKING~GET_POS_PACKING
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_POS_PICKING
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS_PACK
    redefinition .
protected section.
*"* protected components of class ZCL_LGTICA_RESERVA_INV
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_RESERVA_INV
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_RESERVA_INV IMPLEMENTATION.


METHOD zif_lgtica_documento~generar_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Crea Informacion del Documento en Tablas de Picking
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 19.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 19.08.2014    ER6K907188    Marco Suarez G      Creación
*-------------------------------------------------------------------------------*

*.....Variable con Ref a la Interfaz
  DATA:  lo_pick TYPE REF TO zif_lgtica_pickable,
*....Estructura Mensajes de Respuesta
         les_mensaje TYPE zesd_msg_picking.

*.....Up Cast
  lo_pick = me.
*....
  CALL METHOD zcl_lgtica_picking=>create_by_pkble_ref
    EXPORTING
      i_ref_pickable = lo_pick.
*.....Actualizo Base de Datos
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

*.....Mensaje de Confirmacion de Datos en Tablas de Picking
  les_mensaje-num_doc = i_documento_origen.
  les_mensaje-msg = 'Se Guardo Información en Tablas de Picking Exitosamente'.
  les_mensaje-type_msg = 'S'.

  APPEND les_mensaje TO c_mensajes.

ENDMETHOD.


  method ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS_PACK.
*CALL METHOD SUPER->ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS_PACK
*  EXPORTING
*    I_DOCUMENTO_ORIGEN =
**    i_clase_documento  =
**  CHANGING
**    c_mensajes         =
**    c_entregas         =
*    .
*{   INSERT         ER3K900332                                        1
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

*... Cabecera de cabecera documento de picking
  DATA: r_header_picking TYPE zttsd_picking.
*... Estructura para la cabecera del documento de picking
  DATA: les_header_picking TYPE zedsd_picking,
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
*.....Variable con Entrega
        les_entregas TYPE zesd_detalles_entregas,
*.... Estructuras para buscar las reservas en la tabla resb
       lti_resb TYPE STANDARD TABLE OF ty_resb,
        les_resb TYPE ty_resb,
*.... Destinatario de mercancias
        l_s_KUNWE type KUNWE,
*.....Mensaje
       les_msg_picking TYPE zesd_msg_picking,
       l_e_msg TYPE string.

  FIELD-SYMBOLS:
                 <lfs_picking_det> TYPE zedsd_picking_det,
         <lfs_detalle_reservas> TYPE bapi2093_res_items_get,
         <lfs_detalle> TYPE zesd_eanmat,
         <lfs_return> TYPE bapiret2.
*... Estructuras para consulta de la tabla de entregas
data: ls_likp type likp,
       lt_lips type table of lips,
       ls_lips like line of lt_lips.
*       ls_t001w like t001w.

 data: lt_dates type table of BAPIDLVDEADLN,
       ls_dates like line of lt_dates,
       lt_DLV_ITEMS type table of BAPIDLVNOREFITEM,
       ls_dlv_items like line of lt_DLV_ITEMS.
 data: ls_return type bapiret2,
       lt_return type table of bapiret2,
       ld_par1 type SY-MSGV1 ,
       ld_par2 type SY-MSGV1 .
 data: lt_CREATED_ITEMS Type TABLE OF  BAPIDLVITEMCREATED,
       ls_created_items LIKE LINE OF lt_CREATED_ITEMS.
* data ld_vbtyv like lips-vbtyv.
 data: ld_prctr type prctr,
*       ld_kostl like zsdceco_centro-kostl,
       o_vbeln type VBELN_VL.
*       ld_werks like lips-werks,
*       ld_werks_o like lips-werks.}

 ls_dates-timetype = 'WSHDRWADAT'. "WS GOODS ISSUE
 data ls_time(14).
 concatenate sy-datlo sy-timlo into ls_time.
 move ls_time to ls_dates-timestamp_utc.
 ls_dates-timezone = SY-ZONLO.
 append ls_dates to lt_dates.

*.....Reservervas de Inventario
      CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
        EXPORTING
          i_rsnum   = cabecera_reservas-rsnum
        RECEIVING
          e_picknum = l_e_piknum.

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
  IF  les_cab_picking-estado EQ 'CERRADO'.

*....  Reservas
      SELECT  rsnum rspos rsart matnr werks lgort charg bdmng meins aufnr bwart wempf shkzg
        INTO TABLE lti_resb
        FROM resb
        WHERE rsnum EQ cabecera_reservas-rsnum
          AND xloek EQ ''
          and XWAOK eq 'X'
          and werks EQ DETALLES_USUARIO-centro.

*.....Datos Detalle de Picking
      IF c_det_posiciones is INITIAL.
        lti_det_picking = lo_lgtica_picking->t_picking_det.
      ELSE.
        lti_det_picking =   c_det_posiciones.
      ENDIF.

*.....Ordeno Tabla
      SORT lti_det_picking BY posicion ASCENDING.

*.....Recorro Tabla de Posiciones Picking
      LOOP AT lti_det_picking ASSIGNING <lfs_picking_det> where p_confirm EQ 'CT'.

        ls_DLV_ITEMS-REF_ITem = <lfs_picking_det>-pospick.
        ls_DLV_ITEMS-MATERIAL  = 'MATERIALES'."ls_LIPS-MATNR.
        ls_DLV_ITEMS-ITEM_categ = 'ZDL1'.
        ls_DLV_ITEMS-DLV_QTY = <lfs_picking_det>-cantcont.
        ls_DLV_ITEMS-sales_unit = 'ST'."¿ls_lips-meins'.

        ls_DLV_ITEMS-short_text = <lfs_picking_det>-material.
        READ TABLE lti_resb into les_resb WITH KEY rsnum = cabecera_reservas-rsnum rspos = <lfs_picking_det>-posicion.
        IF sy-subrc eq 0.
          ls_DLV_ITEMS-plant = les_resb-werks."'1200'."i_werks'.
        ls_DLV_ITEMS-stge_loc = les_resb-lgort."'1210'.
        ENDIF.
     append ls_DLV_ITEMS to lt_DLV_ITEMS.
      ENDLOOP.
  ELSE.
*      CONCATENATE 'El Documento' space i_documento_origen space
*      'No esta Contabilizado y/o No esta Registrado en la Tabla de Picking' space INTO l_e_msg RESPECTING BLANKS.
*      les_msg_picking-msg = l_e_msg.
      MESSAGE S003(ZCLPACK) WITH i_documento_origen into les_msg_picking-msg.
      les_msg_picking-type_msg = 'E'.
      APPEND les_msg_picking TO c_mensajes.
      CLEAR : les_msg_picking.
      RETURN.
  ENDIF.
*.... Ajusto el destinatario de mercancías
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = cabecera_reservas-wempf
       IMPORTING
         OUTPUT        = l_s_KUNWE
               .
*.... Creo entrega sin referencia
     CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATENOREF'
       EXPORTING
         SHIP_POINT           = ls_DLV_ITEMS-plant"'1200' "ls_LIKP-VSTEL puesto de expedición
         DLV_TYPE             = 'ZLO'
         SALESORG             = '1000' "ls_t001w-vkorg'
         DISTR_CHAN           = '10'"ls_t001w-vtweg
         DIVISION             = '00'"ls_t001w-spart
         SHIP_TO              = l_s_KUNWE"'0000400731'"ls_t001w-kunnr
*         DATE_USAGE           = '0'
*         DEBUG_FLG            =
      IMPORTING
        DELIVERY             = o_vbeln
*         NUM_DELIVERIES       =
       TABLES
         DATES                = lt_dates
         DLV_ITEMS            = lt_dlv_items
*         SERIAL_NUMBERS       =
*         EXTENSION_IN         =
*         DELIVERIES           =
         CREATED_ITEMS        = lt_created_items
*         EXTENSION_OUT        =
         RETURN               = lt_return
               .
          LOOP AT lt_created_items into ls_created_items.
           les_entregas-documento =  cabecera_reservas-rsnum.
           les_entregas-entrega =  ls_created_items-deliv_numb.
           APPEND les_entregas TO c_entregas.
           CLEAR : les_entregas.
         ENDLOOP.
         CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             WAIT   = 'X'
           IMPORTING
             RETURN = ls_return.
*}   INSERT
  endmethod.


METHOD zif_lgtica_documento~get_cabecera.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Reservas de inventario
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------*
* Definición de Constantes Globales
*-------------------------------------------------------------------------------*
*CONSTANT: ………………….
*……………..


*-------------------------------------------------------------------------------*
* Definición de Tipos Locales
*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ty_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
*{   INSERT         ER3K900279                                        8
        enmng type resb-enmng,
*}   INSERT
        aufnr TYPE resb-aufnr,
        BWART type resb-BWART,
*{   INSERT         ER3K900279                                        1
        UMWRK type resb-UMWRK,
        kzear type resb-UMWRK,
        seriales TYPE	c LENGTH 1 ,            "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
        numero_lote TYPE c LENGTH 1,          "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote

*}   INSERT
       END OF ty_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ty_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ty_afko.

*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*{   INSERT         ER3K900279                                        3
*.... Tipo para la descripción del material
  TYPES: BEGIN OF ltp_makt,
          matnr TYPE resb-matnr,
          maktx TYPE makt-maktx,
         END OF ltp_makt.
*}   INSERT

*-------------------------------------------------------------------------------*
* Definición de Estructuras Tables Globales
*-------------------------------------------------------------------------------*
*TABLES: t001,   "Códigos de Compañía
*        kna1,   "Maestro de Clientes

*-------------------------------------------------------------------------------*
* Definición de Estructuras Globables
*-------------------------------------------------------------------------------*
  DATA:
*.....Variable Estructura con Información del Documento
          les_cab_ref TYPE zesd_cab_ref.
*... Estructura para la reserva
  DATA: les_resb TYPE ty_resb.
*... Estructura para las ordenes
  DATA: les_afko TYPE ty_afko.
*... Estructura para las cabecera de ventas
  DATA: les_vbak TYPE ltp_vbak.
*... Estructura para las datos de cliente
  DATA: les_kna1 TYPE ltp_kna1.
*.....Variable Estructura para Mensajes
  DATA:    les_msg TYPE zesd_msg_picking.
*{   INSERT         ER3K900279                                        2
*.... Estructura interna para la descripción del material
 DATA:  les_makt TYPE ltp_makt.

*... Estructura para la reserva
  DATA: les_resbz TYPE ty_resb.
*}   INSERT

*-------------------------------------------------------------------------------*
* Definición de Tablas Internas Globles
**-------------------------------------------------------------------------------*
*... Tabla internae para las reservas
  DATA: lti_resb TYPE STANDARD TABLE OF ty_resb,
*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ty_afko,
*.....Tabla para Detalle Cabecera de Ventas
        lti_vbak TYPE TABLE OF ltp_vbak,
*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,
*{   INSERT         ER3K900279                                        4
  lti_resbz TYPE STANDARD TABLE OF ty_resb,
*.... Tabla interna para la descripción del material
        lti_makt TYPE TABLE OF ltp_makt,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*}   INSERT

*.....Rango para Fechas de Creacion de reservas
          rl_erdat TYPE RANGE OF  afko-gltrp,
          ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
*{   REPLACE        ER3K900279                                        5
*\          lti_actividad TYPE TABLE OF zmm_t_clase_pv.
lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,

*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable para Unidad de medida despues de la Conversion
         lv_meinh TYPE string,

*.....Variable Texto Error
         l_e_desc_error TYPE string.
*Tabla interna de ubicación
  DATA: lti_ubicacion TYPE STANDARD TABLE OF zmm_t_ubicacion.
*... Tabla interna para consultar la tabla de ubicaciones
data: ti_zmm_t_ubicacion type standard table of zmm_t_ubicacion.
*...
data: les_ubicacion type  zmm_t_ubicacion.

data: les_ubicacion_suge type  ZESD_UBIC_SUG.

*}   REPLACE

*-------------------------------------------------------------------------------*
* Definición de Variables Globales
*-------------------------------------------------------------------------------*
*{   INSERT         ER3K900279                                        6

  FIELD-SYMBOLS : <lfs_resb> TYPE ty_resb,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.
*}   INSERT
  DATA: l_o_rsnum TYPE resb-rsnum.
  DATA:
*.....Fecha Inicio
             l_e_fecha_inicio TYPE d,
*.....Fecha Final
             l_e_fecha_fin TYPE d.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "RI
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.

      IF i_documento_origen IS NOT INITIAL .

        move i_documento_origen to l_o_rsnum .
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_o_rsnum
    IMPORTING
      output = l_o_rsnum .

*.... Consulto las ordenes

        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE rsnum eq l_o_rsnum.

*.... Consulto la reserva
*{   REPLACE        ER3K900279                                        7
*\        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr BWART
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart UMWRK kzear
*}   REPLACE
         INTO TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
*           KZEAR
*{   REPLACE        ER3K900279                                       20
*\          AND kzear EQ ''
*          AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
           AND werks EQ detalles_usuario-centro.
*{   INSERT         ER3K900279                                       18
         DELETE lti_resb where  bwart eq '301' and UMWRK EQ detalles_usuario-centro.
*}   INSERT

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
*{   REPLACE        ER3K900279                                        9
*\        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr BWART
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart UMWRK kzear
*}   REPLACE
         APPENDING TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
           AND BWART EQ '301'
*           KZEAR
*{   REPLACE        ER3K900279                                       21
*\           AND kzear EQ ''
*           AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
           AND UMWRK EQ detalles_usuario-centro.
*           AND lgort EQ detalles_usuario-almacen.
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
        ls_erdat-sign = 'I'.
        ls_erdat-option = 'BT'.
        ls_erdat-low = l_e_fecha_inicio.
        ls_erdat-high = l_e_fecha_fin.
        APPEND ls_erdat TO rl_erdat.

*.... Consulto las ordenes en el rango de fechas
        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE gltrp IN rl_erdat.

*.... Valido que se hayan encontrado ordenes
        IF sy-subrc EQ 0.
*.... Ordeno la tabal de ordenes
          SORT lti_afko by rsnum aufnr.
*.... Consulto la reserva
*{   REPLACE        ER3K900279                                       10
*\          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr BWART
          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart UMWRK kzear
*}   REPLACE
            INTO TABLE lti_resb
            FROM resb
            FOR ALL ENTRIES IN lti_afko
            WHERE rsnum EQ lti_afko-rsnum
              AND xloek EQ ''
*{   REPLACE        ER3K900279                                       22
*\              AND kzear EQ ''
*              AND kzear EQ 'X'
*}   REPLACE
*     Filtro dependiendo del Usuario de la consulta
              AND werks EQ detalles_usuario-centro.
*              AND lgort EQ detalles_usuario-almacen.
*{   INSERT         ER3K900279                                       19
            DELETE lti_resb where  bwart eq '301' and UMWRK EQ detalles_usuario-centro.
*}   INSERT

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
*{   REPLACE        ER3K900279                                       11
*\        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr BWART
SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart UMWRK kzear
*}   REPLACE
         APPENDING TABLE lti_resb
         FROM resb
         FOR ALL ENTRIES IN lti_afko
         WHERE rsnum EQ lti_afko-rsnum

           AND xloek EQ ''
           AND BWART EQ '301'
*           KZEAR
*{   REPLACE        ER3K900279                                       23
*\           AND kzear EQ ''
*           AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
           AND UMWRK EQ detalles_usuario-centro.
        ENDIF.
      ENDIF.
*{   INSERT         ER3K900279                                       12
      DELETE lti_resb where  bwart eq '262'.
*}   INSERT

      IF lti_resb[] IS NOT  INITIAL.
*{   INSERT         ER3K900279                                       13
*.... Consulto la descripción de los materiales
          SELECT matnr maktx
            INTO TABLE lti_makt
            FROM makt
            FOR ALL ENTRIES IN lti_resb
            WHERE matnr EQ lti_resb-matnr.

*   Extraemos las ubicaciones, cantidades y unidades de medida del material
*   y lo llevamos a la tabla que se exportará
    SELECT material ubicacion UBIC_DEFAULT cantidad unidad_med
    INTO CORRESPONDING FIELDS OF TABLE lti_ubicacion
    FROM zmm_t_ubicacion
    FOR ALL ENTRIES IN lti_resb
    WHERE centro   EQ lti_resb-WERKS
      AND almacen  EQ lti_resb-LGORT
      AND material EQ lti_resb-matnr.




*}   INSERT
*.... Ordeno las reservas
        SORT lti_resb by rsnum aufnr ascending.
*.... Elimino duplicados
*{   DELETE         ER3K900279                                       16
*\        DELETE ADJACENT DUPLICATES FROM lti_resb comparing rsnum aufnr.
*}   DELETE

*{   REPLACE        ER3K900279                                       14
*\        LOOP AT lti_resb into les_resb.
        lti_resbz = lti_resb.
        DELETE ADJACENT DUPLICATES FROM lti_resbz COMPARING rsnum aufnr.
        LOOP AT lti_resbz into les_resbz.
*}   REPLACE
*.....Busco Información de Cabecera del Pedido de Ventas
          SELECT SINGLE vbeln erdat auart kunnr
               FROM vbak
                  INTO les_vbak
*{   REPLACE        ER3K900279                                       15
*\                    WHERE aufnr EQ les_resb-aufnr. " AND
                    WHERE aufnr EQ les_resbz-aufnr. " AND

            READ TABLE lti_resb into les_resb WITH KEY aufnr = les_resbz-aufnr rsnum = les_resbz-rsnum.

*}   REPLACE
*                              auart EQ <lfs_actividad>-cod_clase.
*.....Consulto Información del Cliente
          SELECT SINGLE kunnr name1
               FROM  kna1
                   INTO les_kna1
                        WHERE kunnr EQ les_vbak-kunnr.

          les_cab_ref-num_doc     = les_resb-rsnum.
          les_cab_ref-fecha_crea  = les_resb-bdter.
          les_cab_ref-tipo_doc    = les_resb-BWART.
          les_cab_ref-cod_cliente = les_vbak-kunnr.
          les_cab_ref-nom_cliente = les_kna1-name1.
          APPEND les_cab_ref TO c_det_cabecera.
*{   INSERT         ER3K900279                                       17
           IF I_FLAG_AGRUP EQ 'X'.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
         LOOP AT lti_resb ASSIGNING <lfs_resb> where bwart ne '262'.
            les_mara-matnr = <lfs_resb>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = c_eans
              t_mara = lti_mara.


*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

SORT lti_resb by matnr ASCENDING.

          DELETE lti_resb where ENMNG = 0 and KZEAR = 'X'.
        LOOP AT lti_resb ASSIGNING <lfs_resb> where bwart ne '262' and rsnum eq les_resbz-RSNUM.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_resb>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_resb>-werks
                  lgort     = <lfs_resb>-lgort
                  username  = i_usuario
                  I_TIPO_DOCUMENTO = I_TIPO_DOCUMENTO
                TABLES
                  t_return  = lti_return
                  t_serno   = lti_serno     "Serie para el Material
                  t_charg   = lti_charg     "Lote para el Material
                  t_estatin = lti_estatin
                  t_matnr   = lti_matnr.

*.....Verifico que no Existan Errores
              READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.

              IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico que el Material Tenga Número de Serie
                IF lti_serno IS NOT INITIAL.
                  APPEND LINES OF lti_serno TO c_seriales.
                  <lfs_resb>-seriales = 'X'.
                ELSE.
                  <lfs_resb>-seriales = ' '.
                ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                IF lti_charg  IS NOT INITIAL  .
                  APPEND LINES OF lti_charg TO c_lotes.
                  <lfs_resb>-numero_lote = 'X'.
                ELSE.
                  <lfs_resb>-numero_lote = ' '.
                ENDIF.
                                ENDIF.
IF I_FLAG_AGRUP is not initial  .
  clear:les_det_pos.
les_det_pos-num_entrega = <lfs_resb>-rsnum.
*                  les_det_pos-pos_entrega = <lfs_resb>-rspos.
                  les_det_pos-material = <lfs_resb>-matnr.

*... leo la descripción del material
                  READ TABLE lti_makt into les_makt with key matnr = <lfs_resb>-matnr.

                  les_det_pos-desc_material = les_makt-maktx.
                  les_det_pos-cant_por_pick = <lfs_resb>-bdmng.
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
                      input          = <lfs_resb>-meins
                      language       = 'S'
                    IMPORTING
                      output         = lv_meinh
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF lv_meinh EQ '004'.
                    les_det_pos-uni_med_doc = <lfs_resb>-meins.
                  ELSE.
                    les_det_pos-uni_med_doc = lv_meinh.
                  ENDIF.

                  les_det_pos-seriales = <lfs_resb>-seriales.
                  les_det_pos-numero_lote = <lfs_resb>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_resb>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.

else.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.

*.....Función para Obtener EANs de un Material
            CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
              EXPORTING
                matnr      = <lfs_resb>-matnr
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
                  les_det_pos-num_entrega = <lfs_resb>-rsnum.
                  les_det_pos-pos_entrega = <lfs_resb>-rspos.
                  les_det_pos-material = <lfs_resb>-matnr.

*... leo la descripción del material
                  READ TABLE lti_makt into les_makt with key matnr = <lfs_resb>-matnr.

                  les_det_pos-desc_material = les_makt-maktx.
                  les_det_pos-cant_por_pick = <lfs_resb>-bdmng.
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
                      input          = <lfs_resb>-meins
                      language       = 'S'
                    IMPORTING
                      output         = lv_meinh
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF lv_meinh EQ '004'.
                    les_det_pos-uni_med_doc = <lfs_resb>-meins.
                  ELSE.
                    les_det_pos-uni_med_doc = lv_meinh.
                  ENDIF.

                  les_det_pos-seriales = <lfs_resb>-seriales.
                  les_det_pos-numero_lote = <lfs_resb>-numero_lote.
*.....Datos de Detalle EAN
                  les_det_pos-ean = <lfs_detalle>-ean.
                  les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                  les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                  APPEND les_det_pos TO c_det_posiciones.
                ENDLOOP.
              ELSE.
                les_msg-num_doc = ' '.
                CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_resb>-matnr
                INTO l_e_desc_error SEPARATED BY space.
                les_msg-msg = l_e_desc_error.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
              ENDIF.
            ENDIF.
ENDIF.

        ENDLOOP.
           ENDIF.
*}   INSERT

        ENDLOOP.
      ELSE.
        les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
      ENDif.
    ELSE.
         les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
    ENDIF.
  ENDIF.
ENDMETHOD.


method ZIF_LGTICA_DOCUMENTO~GET_CABECERA_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Entrega
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ltp_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
        aufnr TYPE resb-aufnr,
        bwart TYPE resb-bwart,
*{   INSERT         ER3K900332                                       20
        WEMPF type resb-WEMPF,
*}   INSERT
       END OF ltp_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ty_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ty_afko.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.
  DATA :
*... Estructura para las cabecera de ventas
     les_vbak TYPE ltp_vbak,
*.....Variable Estructura con Información del Documento
     les_cab_ref TYPE zesd_cab_ref,
*... Estructura para las ordenes
     les_afko TYPE ty_afko,

*.....Tabla Interna para Datos de reserva
        lti_resb TYPE TABLE OF ltp_resb,
        les_resb TYPE ltp_resb,
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking TYPE TABLE OF zmm_t_picking,
        les_picking TYPE zmm_t_picking,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing TYPE TABLE OF zmm_t_packing,
        les_packing TYPE zmm_t_packing,
*{   INSERT         ER3K900332                                        8
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking_dt TYPE TABLE OF zmm_t_picking_dt,
        les_picking_dt TYPE zmm_t_picking_dt,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing_po TYPE TABLE OF zmm_t_packing_po,
        les_packing_po TYPE zmm_t_packing_po,

*... Tablas para el manejo del trasporte
  lti_packing_tr TYPE STANDARD TABLE OF zmm_t_transporte,
        les_packing_tr TYPE zmm_t_transporte,
        E_MENSAJES  TYPE  ZTTSD_MSG_PICKING,
        E_ENTREGAS  TYPE  ZTTSD_ENTREGAS ,
*.....Variable con Entrega
        les_entregas TYPE zesd_detalles_entregas,

*}   INSERT

*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ty_afko,

*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,
        les_kna1 TYPE ltp_kna1,
*.....Variable Estructura para Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Rango para Fechas de Creacion de Pedidos
        rl_erdat TYPE RANGE OF  vbak-erdat,
        ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Fecha Inicio
           l_e_fecha_inicio TYPE d,
           l_o_rsnum TYPE resb-rsnum,
           lv_kunnr TYPE kunnr,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_picking> TYPE zmm_t_picking,
*{   INSERT         ER3K900332                                       23
                 <lfs_picking_dt> TYPE zmm_t_picking_dt,
                 <lfs_packing_po> TYPE zmm_t_packing_po,
*}   INSERT
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_resb> TYPE ltp_resb,
                 <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "PVTA
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.

      IF i_documento_origen IS NOT INITIAL AND i_cod_cliente IS INITIAL.
        MOVE i_documento_origen TO l_o_rsnum.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_o_rsnum
          IMPORTING
            output = l_o_rsnum.
*{   INSERT         ER3K900332                                        9
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = i_documento_origen
          IMPORTING
            output = i_documento_origen.

*}   INSERT

*.... Consulto las ordenes

        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE rsnum EQ l_o_rsnum.

*.... Consulto la reserva
*{   REPLACE        ER3K900332                                       16
*\        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart WEMPF
*}   REPLACE
         INTO TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
*           KZEAR
*{   REPLACE        ER3K900332                                        1
*\          AND kzear EQ ''
*          AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
           AND werks EQ detalles_usuario-centro.

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
*{   REPLACE        ER3K900332                                       17
*\        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart WEMPF
*}   REPLACE
         APPENDING TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
           AND bwart EQ '301'
*           KZEAR
*{   REPLACE        ER3K900332                                        2
*\           AND kzear EQ ''
*           AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
           AND umwrk EQ detalles_usuario-centro.


      ELSEIF i_cod_cliente IS NOT INITIAL.
        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.


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
        ls_erdat-sign = 'I'.
        ls_erdat-option = 'BT'.
        ls_erdat-low = l_e_fecha_inicio.
        ls_erdat-high = l_e_fecha_fin.
        APPEND ls_erdat TO rl_erdat.

*.... Consulto las ordenes en el rango de fechas
        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE gltrp IN rl_erdat.

*.... Valido que se hayan encontrado ordenes
        IF sy-subrc EQ 0.
*.... Ordeno la tabal de ordenes
          SORT lti_afko BY rsnum aufnr.
*.... Consulto la reserva
*{   REPLACE        ER3K900332                                       18
*\          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart WEMPF
*}   REPLACE
            INTO TABLE lti_resb
            FROM resb
            FOR ALL ENTRIES IN lti_afko
            WHERE rsnum EQ lti_afko-rsnum
              AND xloek EQ ''
*{   REPLACE        ER3K900332                                        3
*\              AND kzear EQ ''
*              AND kzear EQ 'X'
*}   REPLACE
*     Filtro dependiendo del Usuario de la consulta
              AND werks EQ detalles_usuario-centro.
*              AND lgort EQ detalles_usuario-almacen.

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
*{   REPLACE        ER3K900332                                       19
*\          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart WEMPF
*}   REPLACE
           APPENDING TABLE lti_resb
           FROM resb
           FOR ALL ENTRIES IN lti_afko
           WHERE rsnum EQ lti_afko-rsnum

             AND xloek EQ ''
             AND bwart EQ '301'
*           KZEAR
*{   REPLACE        ER3K900332                                        4
*\             AND kzear EQ ''
*             AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
             AND umwrk EQ detalles_usuario-centro.
        ENDIF.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_resb[] IS NOT  INITIAL.
*{   REPLACE        ER3K900332                                        6
*\*.....verificar documentos despachados para entregas
*\        SELECT *
*\           FROM zmm_t_packing
*\             INTO TABLE lti_packing
*\               FOR ALL ENTRIES IN lti_resb
*\                  WHERE rsnum  EQ lti_resb-rsnum AND
*\                        estado EQ 'CERRADO'.
*\
*\*.....Busqueda de documentos Pendientes por Packing para entregas
*\        SELECT *
*\           FROM zmm_t_picking
*\             INTO TABLE lti_picking
*\               FOR ALL ENTRIES IN lti_resb
*\                  WHERE rsnum EQ lti_resb-rsnum AND
*\                        estado EQ 'CERRADO'.
*.....verificar documentos despachados para entregas
           SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR  RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD
                  USRMOD UNAMOD ESTADO
           FROM zmm_t_packing
             INTO TABLE lti_packing
               FOR ALL ENTRIES IN lti_resb
                  WHERE rsnum  EQ lti_resb-rsnum AND
                        estado NE 'CERRADO'.

*.....Busqueda de documentos Pendientes por Packing para entregas
        SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD
               USRMOD UNAMOD ESTADO
           FROM zmm_t_picking
             INTO TABLE lti_picking
               FOR ALL ENTRIES IN lti_resb
                  WHERE rsnum EQ lti_resb-rsnum AND
                        usuario eq detalles_usuario-usuario and
                        estado EQ 'CERRADO'.


 CALL METHOD zcl_lgtica_transporte=>get_data_by_doc
      EXPORTING
        i_tipo_doc   = i_tipo_documento
        i_documento  = i_documento_origen
      IMPORTING
        e_transporte = lti_packing_tr
      CHANGING
        c_mensajes   = c_mensajes.


     IF I_entrega is not INITIAL.
       les_packing_tr-vbeln = I_entrega.
     ELSEIF lti_packing_tr IS NOT INITIAL and lti_picking IS NOT INITIAL.
       IF lti_packing is not INITIAL.
*.... Consultamos las posiciones de picking
            SELECT  MANDT POSICION PACKNUM POSPACK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT
                  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO UMANIPULACION UMANIPULACION_SE UEMBALAJE
            FROM zmm_t_packing_po
            INTO TABLE lti_packing_po
            FOR ALL ENTRIES IN lti_packing
            WHERE packnum EQ lti_packing-packnum.
       ENDIF.
       IF lti_picking is not INITIAL.
*.... Consultamos las posiciones de picking
            SELECT  MANDT POSICION PICKNUM POSPICK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU
                  UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT  UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM
                  AUFNR USUARIO
            FROM zmm_t_picking_dt
            INTO TABLE lti_picking_dt
            FOR ALL ENTRIES IN lti_picking
            WHERE picknum EQ lti_picking-picknum.
       ENDIF.
*.... Picking parciales
      SORT lti_packing_po by material.
      SORT lti_picking_dt by pospick.

      LOOP AT lti_picking_dt ASSIGNING <lfs_picking_dt>.
          READ TABLE lti_packing_po into les_packing_po WITH KEY material = <lfs_picking_dt>-material BINARY SEARCH.
          IF sy-subrc eq 0.
              LOOP AT lti_packing_po ASSIGNING <lfs_packing_po> from sy-tabix .

*.... Valido que este recorriendo el mismo material del primer loop
                IF <lfs_picking_dt>-material <> <lfs_packing_po>-material.
                   EXIT.
                ENDIF.
*.... La cantidad que hay en packing es menor o igual a la de picking
                IF <lfs_packing_po>-cantidad <= <lfs_picking_dt>-cantcont.
*.... Resto a la cantidad de picking la cantidad de packing y pongo en 0 la cantidad de packing
                <lfs_picking_dt>-cantcont = <lfs_picking_dt>-cantcont - <lfs_packing_po>-cantidad.
                <lfs_packing_po>-cantidad = 0.
                ELSE.
*.... La cantidad que hay en packing es mayor a la de picking
*.... Resto a la cantidad de packing la cantidad de picking y pongo en 0 la cantidad de picking
                <lfs_packing_po>-cantidad = <lfs_packing_po>-cantidad - <lfs_picking_dt>-cantcont .
                <lfs_picking_dt>-cantcont = 0.
                ENDIF.

                IF <lfs_picking_dt>-cantcont EQ 0.
                  EXIT.
                ENDIF.
              ENDLOOP.
          ENDIF.
      ENDLOOP.
      DELETE lti_picking_dt where cantcont eq 0.
      IF lti_picking_dt is INITIAL.
        READ TABLE lti_packing_tr INTO les_packing_tr WITH KEY documento = i_documento_origen.
      ELSE.
        CALL FUNCTION 'Z_SD_ENTREGAS_PACK_SUP'
         EXPORTING
           i_documento_origen       = i_documento_origen
           i_tipo_documento         = i_tipo_documento
*          I_CLASE_DOCUMENTO        =
           i_usuario                = detalles_usuario-USUARIO

        IMPORTING
          E_MENSAJES               = E_MENSAJES
          E_ENTREGAS               = E_ENTREGAS
        CHANGING
           picking_det_sup          = lti_picking_dt         .
*.....Elimino Registros Duplicados
      DELETE ADJACENT DUPLICATES FROM E_ENTREGAS COMPARING documento entrega.
      READ TABLE e_entregas into les_entregas INDEX 1.
      les_packing_tr-vbeln = les_entregas-entrega.
      ENDIF.


******

     ELSE.
       CALL FUNCTION 'Z_SD_ENTREGAS_PACK_SUP'
         EXPORTING
           i_documento_origen       = i_documento_origen
           i_tipo_documento         = i_tipo_documento
*          I_CLASE_DOCUMENTO        =
           i_usuario                = detalles_usuario-USUARIO
        IMPORTING
          E_MENSAJES               = E_MENSAJES
          E_ENTREGAS               = E_ENTREGAS
                 .
*.....Elimino Registros Duplicados
      DELETE ADJACENT DUPLICATES FROM E_ENTREGAS COMPARING documento entrega.
      READ TABLE e_entregas into les_entregas INDEX 1.
      les_packing_tr-vbeln = les_entregas-entrega.
     ENDIF.
*}   REPLACE


*.....Verifico que Existan Registros Pendientes por Packing Campo
        IF sy-subrc NE 0.
*{   REPLACE        ER3K900332                                       10
*\          les_msg-msg = 'No Existen Documentos Pendientes por Packing'.
          MESSAGE S001(ZCLPACK) into les_msg-msg .
*}   REPLACE
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ELSE.
*.....Busco Información para la Clase de Documento
          READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
          IF sy-subrc EQ 0 .
*{   INSERT         ER3K900332                                       15
*..... Consulto los datos de la reserva
              READ TABLE lti_resb ASSIGNING <lfs_resb> index 1.

*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lfs_resb>-wempf
          IMPORTING
            output = <lfs_resb>-wempf.
*.....Consulto Información del Cliente
                SELECT SINGLE kunnr name1
                     FROM  kna1
                         INTO les_kna1
                              WHERE kunnr EQ <lfs_resb>-wempf+2(10).
*.... Asigno el cliente a el field symbol
                ASSIGN les_kna1 TO <lfs_kna1>.
*}   INSERT

*.....Recorro Tabla de Cabeceras
            LOOP AT lti_picking ASSIGNING <lfs_picking>.
*{   DELETE         ER3K900332                                       22
*\              READ TABLE lti_packing WITH KEY rsnum = <lfs_picking>-rsnum INTO les_packing.
*\              IF sy-subrc EQ 0.
*\                CONTINUE.
*\              ENDIF.
*}   DELETE
              READ TABLE lti_resb WITH KEY rsnum = <lfs_picking>-rsnum
                                  ASSIGNING <lfs_resb>.
              IF <lfs_resb> IS ASSIGNED.

*{   DELETE         ER3K900332                                       14
*\*.....Busco Información de Cabecera del Pedido de Ventas
*\                SELECT SINGLE vbeln erdat auart kunnr
*\                     FROM vbak
*\                        INTO les_vbak
*\                          WHERE aufnr EQ les_resb-aufnr. " AND
*\*                              auart EQ <lfs_actividad>-cod_clase.
*\*.....Consulto Información del Cliente
*\                SELECT SINGLE kunnr name1
*\                     FROM  kna1
*\                         INTO les_kna1
*\                              WHERE kunnr EQ les_vbak-kunnr.
*\*.... Asigno el cliente a el field symbol
*\                ASSIGN les_kna1 TO <lfs_kna1>.
*}   DELETE
*.... Válido que haya encontrado cliente
                IF <lfs_kna1> IS ASSIGNED.
                  IF <lfs_picking>-rsnum IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-rsnum.
                  ENDIF.

*{   DELETE         ER3K900332                                        7
*\                  IF <lfs_picking>-aufnr IS NOT INITIAL.
*\                    les_cab_ref-num_doc = <lfs_picking>-aufnr.
*\                  ENDIF.
*}   DELETE
                  les_cab_ref-fecha_crea  = <lfs_picking>-fecha.
*            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
*{   REPLACE        ER3K900332                                       21
*\                  les_cab_ref-cod_cliente = les_vbak-kunnr.
                  les_cab_ref-cod_cliente = <lfs_resb>-wempf+2(10).
*}   REPLACE
                  les_cab_ref-nom_cliente = <lfs_kna1>-name1.
*{   INSERT         ER3K900332                                        5
                  les_cab_ref-vbeln = les_packing_tr-vbeln.
*}   INSERT
*                  les_cab_ref-vbeln = <lfs_picking>-vbeln2.
                  APPEND les_cab_ref TO c_det_cabecera.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ELSE.
*          Parametrización de la actividad
*{   REPLACE        ER3K900332                                       13
*\            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
            MESSAGE S000(ZCLPACK) into les_msg-msg .
*}   REPLACE
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.
        ENDIF.
      ELSE.
*{   REPLACE        ER3K900332                                       11
*\        les_msg-msg = 'No Existen Documentos Pendientes por Packing'.
        MESSAGE S001(ZCLPACK) into les_msg-msg .
*}   REPLACE
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
    ELSE.
*{   REPLACE        ER3K900332                                       12
*\      les_msg-msg = 'Debe Ingresar Información de Fechas y/o Número de Documento'.
      MESSAGE S002(ZCLPACK) into les_msg-msg .
*}   REPLACE
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.

  ENDIF.

endmethod.


METHOD zif_lgtica_documento~get_detalle.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Reservas de inventario
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------*
* Definición de Constantes Globales
*-------------------------------------------------------------------------------*
*CONSTANT: ………………….
*……………..


*-------------------------------------------------------------------------------*
* Definición de Tipos Locales
*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ty_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
        enmng TYPE resb-enmng,
        aufnr TYPE resb-aufnr,
        bwart TYPE resb-bwart,
          seriales TYPE	c LENGTH 1 ,            "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1,          "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote

       END OF ty_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ty_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ty_afko.

*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*.... Tipo para la descripción del material
  TYPES: BEGIN OF ltp_makt,
          matnr TYPE resb-matnr,
          maktx TYPE makt-maktx,
         END OF ltp_makt.
*-------------------------------------------------------------------------------*
* Definición de Estructuras Tables Globales
*-------------------------------------------------------------------------------*
*TABLES: t001,   "Códigos de Compañía
*        kna1,   "Maestro de Clientes

*-------------------------------------------------------------------------------*
* Definición de Estructuras Globables
*-------------------------------------------------------------------------------*
  DATA:
*.....Variable Estructura con Información del Documento
          les_cab_ref TYPE zesd_cab_ref.
*... Estructura para la reserva
  DATA: les_resb TYPE ty_resb.
*... Estructura para las ordenes
  DATA: les_afko TYPE ty_afko.
*... Estructura para las cabecera de ventas
  DATA: les_vbak TYPE ltp_vbak.
*... Estructura para las datos de cliente
  DATA: les_kna1 TYPE ltp_kna1.
*.... Estructura interna para la descripción del material
 DATA:  les_makt TYPE ltp_makt.
*.....Variable Estructura para Mensajes
  DATA:    les_msg TYPE zesd_msg_picking.
DATA: l_o_rsnum TYPE resb-rsnum.
*-------------------------------------------------------------------------------*
* Definición de Tablas Internas Globles
**-------------------------------------------------------------------------------*
*... Tabla internae para las reservas
  DATA: lti_resb TYPE STANDARD TABLE OF ty_resb,
*.... Tabla interna para la descripción del material
        lti_makt TYPE TABLE OF ltp_makt,
*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ty_afko,
*.....Tabla para Detalle Cabecera de Ventas
        lti_vbak TYPE TABLE OF ltp_vbak,
*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,
*.....Tablas Retorno de Función para Busqueda de Lotes y Seriales
         lti_return TYPE TABLE OF bapiret1,
         lti_serno  TYPE TABLE OF zstmm_0003,
         lti_charg TYPE TABLE OF zstmm_0002,
         lti_estatin TYPE TABLE OF zstmm_0001,
         lti_matnr TYPE TABLE OF zstmm_0004,
*.....Rango para Fechas de Creacion de reservas
          rl_erdat TYPE RANGE OF  afko-gltrp,
          ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
          lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable Estructura para Detalle de Posiciones
         les_det_pos TYPE zesd_det_ref,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
         lti_detalle TYPE TABLE OF zesd_eanmat,
         lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable para Unidad de medida despues de la Conversion
         lv_meinh TYPE string,

*.....Variable Texto Error
         l_e_desc_error TYPE string.

*-------------------------------------------------------------------------------*
* Definición de Variables Globales
*-------------------------------------------------------------------------------*

  FIELD-SYMBOLS : <lfs_resb> TYPE ty_resb,
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking.
  DATA:
*.....Fecha Inicio
             l_e_fecha_inicio TYPE d,
*.....Fecha Final
             l_e_fecha_fin TYPE d.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "RI
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.

      IF i_documento_origen IS NOT INITIAL .

        move i_documento_origen to l_o_rsnum .
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_o_rsnum
    IMPORTING
      output = l_o_rsnum .

*.... Consulto las ordenes
        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE rsnum EQ l_o_rsnum.

*.... Consulto la reserva
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
         INTO CORRESPONDING FIELDS OF TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
*           KZEAR
          AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
           AND werks EQ detalles_usuario-centro.

*.... Consulto la reserva
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
         appending CORRESPONDING FIELDS OF TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
*           KZEAR
          AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
           AND UMWRK EQ detalles_usuario-centro.
*           AND lgort EQ detalles_usuario-almacen.
        IF sy-subrc EQ 0.
*.... Consulto la descripción de los materiales
          SELECT matnr maktx
            INTO TABLE lti_makt
            FROM makt
            FOR ALL ENTRIES IN lti_resb
            WHERE matnr EQ lti_resb-matnr.
        ENDIF.
      ENDIF.
*{   INSERT         ER3K900304                                        2
DELETE lti_resb where  bwart ne '262'.
*}   INSERT

      IF lti_resb[] IS NOT  INITIAL.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
         LOOP AT lti_resb ASSIGNING <lfs_resb> where bwart ne '262'.
            les_mara-matnr = <lfs_resb>-matnr.
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
*{   INSERT         ER3K900304                                        3
SORT lti_resb by matnr ASCENDING.
*}   INSERT

        LOOP AT lti_resb ASSIGNING <lfs_resb> where bwart ne '262'.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_resb>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_resb>-werks
                  lgort     = <lfs_resb>-lgort
                  username  = i_usuario
                  I_TIPO_DOCUMENTO = I_TIPO_DOCUMENTO
                TABLES
                  t_return  = lti_return
                  t_serno   = lti_serno     "Serie para el Material
                  t_charg   = lti_charg     "Lote para el Material
                  t_estatin = lti_estatin
                  t_matnr   = lti_matnr.

*.....Verifico que no Existan Errores
              READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
*{   REPLACE        ER3K900304                                        4
*\              IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*\*.....Verifico que el Material Tenga Número de Serie
*\                IF lti_serno IS NOT INITIAL.
*\                  APPEND LINES OF lti_serno TO c_seriales.
*\                  <lfs_resb>-seriales = 'X'.
*\                ELSE.
*\                  <lfs_resb>-seriales = ' '.
*\                ENDIF.
*\*                ENDIF.
*\*.....Verifico si el Material Tiene Número de Lote
*\                IF lti_charg  IS NOT INITIAL  .
*\                  APPEND LINES OF lti_charg TO c_lotes.
*\                  <lfs_resb>-numero_lote = 'X'.
*\                ELSE.
*\                  <lfs_resb>-numero_lote = ' '.
*\                ENDIF.
*\*                ENDIF.
*\*.....Limpieza de Variables
*\                CLEAR : lti_detalle, lti_mensajes.
*\
*\*.....Función para Obtener EANs de un Material
*\            CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\              EXPORTING
*\                matnr      = <lfs_resb>-matnr
*\              TABLES
*\                t_detalle  = lti_detalle
*\                t_mensajes = lti_mensajes.
*\
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores
*\            READ TABLE lti_mensajes INDEX 1 ASSIGNING <lfs_mensajes>.
*\            IF sy-subrc EQ 0 .
*\              les_msg-num_doc = <lfs_mensajes>-num_doc.
*\              les_msg-msg = <lfs_mensajes>-msg.
*\              les_msg-type_msg = <lfs_mensajes>-msg.
*\              APPEND les_msg TO c_mensajes.
*\            ELSE.
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\              IF lti_detalle IS NOT INITIAL  .
*\                LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                  les_det_pos-num_entrega = <lfs_resb>-rsnum.
*\                  les_det_pos-pos_entrega = <lfs_resb>-rspos.
*\                  les_det_pos-material = <lfs_resb>-matnr.
*\
*\*... leo la descripción del material
*\                  READ TABLE lti_makt into les_makt with key matnr = <lfs_resb>-matnr.
*\
*\                  les_det_pos-desc_material = les_makt-maktx.
*\                  les_det_pos-cant_por_pick = <lfs_resb>-bdmng.
*\                  les_det_pos-ubic_fija = ' '.
*\                  les_det_pos-ubic_tmp =  ' '.
*\                  les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*\*       Realizar la conversión de la unidad de medida
*\                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                    EXPORTING
*\                      input          = <lfs_resb>-meins
*\                      language       = 'S'
*\                    IMPORTING
*\                      output         = lv_meinh
*\                    EXCEPTIONS
*\                      unit_not_found = 1
*\                      OTHERS         = 2.
*\                  IF lv_meinh EQ '004'.
*\                    les_det_pos-uni_med_doc = <lfs_resb>-meins.
*\                  ELSE.
*\                    les_det_pos-uni_med_doc = lv_meinh.
*\                  ENDIF.
*\
*\                  les_det_pos-seriales = <lfs_resb>-seriales.
*\                  les_det_pos-numero_lote = <lfs_resb>-numero_lote.
*\*.....Datos de Detalle EAN
*\                  les_det_pos-ean = <lfs_detalle>-ean.
*\                  les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                  les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                  APPEND les_det_pos TO c_det_posiciones.
*\                ENDLOOP.
*\              ELSE.
*\                les_msg-num_doc = ' '.
*\                CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_resb>-matnr
*\                INTO l_e_desc_error SEPARATED BY space.
*\                les_msg-msg = l_e_desc_error.
*\                les_msg-type_msg = 'E'.
*\                APPEND les_msg TO c_mensajes.
*\              ENDIF.
*\            ENDIF.
*\          ELSE.
*\            les_msg-num_doc = <lfs_resb>-rsnum.
*\            les_msg-msg = <lfs_return>-message.
*\            les_msg-type_msg = 'E'.
*\            APPEND les_msg TO c_mensajes.
*\          ENDIF.
              IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico que el Material Tenga Número de Serie
                IF lti_serno IS NOT INITIAL.
                  APPEND LINES OF lti_serno TO c_seriales.
                  <lfs_resb>-seriales = 'X'.
                ELSE.
                  <lfs_resb>-seriales = ' '.
                ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                IF lti_charg  IS NOT INITIAL  .
                  APPEND LINES OF lti_charg TO c_lotes.
                  <lfs_resb>-numero_lote = 'X'.
                ELSE.
                  <lfs_resb>-numero_lote = ' '.
                ENDIF.
                                ENDIF.
IF I_FLAG_AGRUP is not initial  .
  clear:les_det_pos.
les_det_pos-num_entrega = <lfs_resb>-rsnum.
*                  les_det_pos-pos_entrega = <lfs_resb>-rspos.
                  les_det_pos-material = <lfs_resb>-matnr.

*... leo la descripción del material
                  READ TABLE lti_makt into les_makt with key matnr = <lfs_resb>-matnr.

                  les_det_pos-desc_material = les_makt-maktx.
                  les_det_pos-cant_por_pick = <lfs_resb>-bdmng.
                  les_det_pos-ubic_fija = ' '.
                  les_det_pos-ubic_tmp =  ' '.
                  les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input          = <lfs_resb>-meins
                      language       = 'S'
                    IMPORTING
                      output         = lv_meinh
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF lv_meinh EQ '004'.
                    les_det_pos-uni_med_doc = <lfs_resb>-meins.
                  ELSE.
                    les_det_pos-uni_med_doc = lv_meinh.
                  ENDIF.

                  les_det_pos-seriales = <lfs_resb>-seriales.
                  les_det_pos-numero_lote = <lfs_resb>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_resb>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.

else.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.

*.....Función para Obtener EANs de un Material
            CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
              EXPORTING
                matnr      = <lfs_resb>-matnr
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
                  les_det_pos-num_entrega = <lfs_resb>-rsnum.
                  les_det_pos-pos_entrega = <lfs_resb>-rspos.
                  les_det_pos-material = <lfs_resb>-matnr.

*... leo la descripción del material
                  READ TABLE lti_makt into les_makt with key matnr = <lfs_resb>-matnr.

                  les_det_pos-desc_material = les_makt-maktx.
                  les_det_pos-cant_por_pick = <lfs_resb>-bdmng.
                  les_det_pos-ubic_fija = ' '.
                  les_det_pos-ubic_tmp =  ' '.
                  les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_ekpo>-meins.
*       Realizar la conversión de la unidad de medida
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input          = <lfs_resb>-meins
                      language       = 'S'
                    IMPORTING
                      output         = lv_meinh
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF lv_meinh EQ '004'.
                    les_det_pos-uni_med_doc = <lfs_resb>-meins.
                  ELSE.
                    les_det_pos-uni_med_doc = lv_meinh.
                  ENDIF.

                  les_det_pos-seriales = <lfs_resb>-seriales.
                  les_det_pos-numero_lote = <lfs_resb>-numero_lote.
*.....Datos de Detalle EAN
                  les_det_pos-ean = <lfs_detalle>-ean.
                  les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                  les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                  APPEND les_det_pos TO c_det_posiciones.
                ENDLOOP.
              ELSE.
                les_msg-num_doc = ' '.
                CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_resb>-matnr
                INTO l_e_desc_error SEPARATED BY space.
                les_msg-msg = l_e_desc_error.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
              ENDIF.
            ENDIF.
ENDIF.
*}   REPLACE
        ENDLOOP.
      ELSE.
        les_msg-msg = 'No Existen Documentos Pendientes por Picking'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
    ELSE.
      les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_documento~get_detalle_packing.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos packing reservas
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ltp_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
        enmng TYPE resb-enmng,
        aufnr TYPE resb-aufnr,
        bwart TYPE resb-bwart,
          seriales TYPE	c LENGTH 1 ,            "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1,          "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote

       END OF ltp_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ltp_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ltp_afko.

*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*.... Tipo para la descripción del material
  TYPES: BEGIN OF ltp_makt,
          matnr TYPE resb-matnr,
          maktx TYPE makt-maktx,
         END OF ltp_makt.

  DATA :
*.... Tabla interna para la descripción del material
        lti_makt TYPE TABLE OF ltp_makt,
*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ltp_afko,
*.....Tabla para Detalle Cabecera de Ventas
        lti_vbak TYPE TABLE OF ltp_vbak,
*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,

*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Información Detalles de Documento Picking
        lti_picking_det TYPE zttsd_picking_det,
*        les_picking_det TYPE zedsd_picking_det,
*.....Información Seriales
        lti_picking_ser TYPE zttsd_picking_ser,
        les_picking_ser TYPE zedsd_picking_ser,
*.....Información Lotes
        lti_picking_lot TYPE zttsd_picking_lot,
        les_picking_lot TYPE zedsd_picking_lot,

*..... objeto de clase picking
         lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Estructura con Datos Principales de reserva
          lti_resb TYPE STANDARD TABLE OF ltp_resb,
          les_resb TYPE ltp_resb,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum,
*.....Variable Estructura para Tabla de Mensajes
           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           les_serno  TYPE zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           les_charg  TYPE zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable para la reserva
       l_o_rsnum TYPE resb-rsnum,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

           lv_vbeln TYPE vbeln,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS :
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_picking_det> TYPE zmm_t_picking_dt,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                  <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "PVTA
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido si Existe Información para el Usuario del Dispositivo
    IF detalles_usuario IS NOT INITIAL.

      MOVE i_documento_origen TO l_o_rsnum .
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_o_rsnum
        IMPORTING
          output = l_o_rsnum.

*.... Consulto las ordenes
      SELECT aufnr gltrp rsnum
      INTO TABLE lti_afko
      FROM afko
      WHERE rsnum EQ l_o_rsnum.

*.... Consulto la reserva
      SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
       INTO CORRESPONDING FIELDS OF TABLE lti_resb
       FROM resb
       WHERE rsnum EQ l_o_rsnum
         AND xloek EQ ''
*           KZEAR
*{   REPLACE        ER3K900332                                        2
*\        AND kzear EQ ''
*        AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
         AND werks EQ detalles_usuario-centro.

*.... Consulto la reserva
      SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
       APPENDING CORRESPONDING FIELDS OF TABLE lti_resb
       FROM resb
       WHERE rsnum EQ l_o_rsnum
         AND xloek EQ ''
*           KZEAR
*{   REPLACE        ER3K900332                                        1
*\        AND kzear EQ ''
*        AND kzear EQ 'X'
*}   REPLACE
*      Filtro dependiendo del Usuario de la consulta
         AND umwrk EQ detalles_usuario-centro.
*           AND lgort EQ detalles_usuario-almacen.
*{   DELETE         ER3K900332                                        4
*\      IF sy-subrc EQ 0.
*\*.... Consulto la descripción de los materiales
*\        SELECT matnr maktx
*\          INTO TABLE lti_makt
*\          FROM makt
*\          FOR ALL ENTRIES IN lti_resb
*\          WHERE matnr EQ lti_resb-matnr.
*\      ENDIF.
*}   DELETE

*.....Verifico que Existan Registros
*{   REPLACE        ER3K900332                                        3
*\      IF sy-subrc EQ 0 .
      IF lti_resb is not INITIAL.
*.... Consulto la descripción de los materiales
        SELECT matnr maktx
          INTO TABLE lti_makt
          FROM makt
          FOR ALL ENTRIES IN lti_resb
          WHERE matnr EQ lti_resb-matnr.
*}   REPLACE
*.... Obtengo el picknum de la reserva
        CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
          EXPORTING
            i_rsnum   = l_o_rsnum
          RECEIVING
            e_picknum = l_e_piknum.


      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.

*.....Verifico que exista picknum a Procesar
      IF l_e_piknum IS NOT INITIAL.
*.....Creo Instancia de Clase
        IF  lo_lgtica_picking IS NOT BOUND  .
          CREATE OBJECT lo_lgtica_picking .
        ENDIF.
*.....Cargo Atributos Picking
        lo_lgtica_picking->load_picking(
          EXPORTING
            i_picknum = l_e_piknum ).

*.....Busco el Codigo de la Clase para El documento Recibido
        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc NE 0 .
          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
*.....Inicia Proceso de Actualización de reserva
*.....Consulto Cabecera Documento Picking
        les_picking_cab = lo_lgtica_picking->s_picking.

        IF  les_picking_cab-estado EQ 'CERRADO' AND
            les_picking_cab IS NOT INITIAL.


*.....Consulto detalle Documento Picking
          lti_picking_det = lo_lgtica_picking->t_picking_det.
*.....Consulto seriales Documento Picking
          lti_picking_ser = lo_lgtica_picking->t_picking_ser.
*.....Consulto lotes Documento Picking
          lti_picking_lot = lo_lgtica_picking->t_picking_lot.
*.... Filtro seriales y lotes de la reserva
*          DELETE lti_picking_lot WHERE vbeln2 NE lv_vbeln.
*          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det>.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = <lfs_picking_det>-material.

            les_mara-matnr = <lfs_picking_det>-material.
            APPEND les_mara TO lti_mara.

          ENDLOOP.
*{   REPLACE        ER3K900332                                        5
*\
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
*{   INSERT         ER3K900332                                        8
          SORT lti_picking_det by material.
*}   INSERT

          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
*{   INSERT         ER3K900332                                        7
           les_det_pos-ubic_sugerida = sy-tabix.
*}   INSERT
*.....Función para Ajuste de Número
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = <lfs_picking_det>-material.

*....    Valido material pertenezca a la reserva ( Centro y Almacen )
*{   REPLACE        ER3K900332                                       10
*\            READ TABLE lti_resb INTO les_resb WITH KEY  matnr = <lfs_picking_det>-material.
            READ TABLE lti_resb INTO les_resb WITH KEY  matnr = <lfs_picking_det>-material rspos = <lfs_picking_det>-posicion.
*}   REPLACE
            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_picking_det>-material.
              APPEND l_e_matnr TO lti_matnr.

*.....Verifico si el Material Tiene Número de Lote

              LOOP AT lti_picking_lot  INTO les_picking_lot WHERE matnr EQ <lfs_picking_det>-material.

                les_charg-matnr = <lfs_picking_det>-material.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = <lfs_picking_det>-material
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.
                UNASSIGN <lfs_detalle>.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lot-umc.
                IF sy-subrc EQ 0.
                  les_charg-clabs = les_picking_lot-cantidad * <lfs_detalle>-cantidad_ean.
                ELSE.
                  les_charg-clabs = les_picking_lot-cantidad.
                ENDIF.
                les_charg-charg = les_picking_lot-lote.

                APPEND les_charg TO lti_charg.
              ENDLOOP.

*.....Verifico si el Material Tiene Número de serie

              LOOP AT lti_picking_ser  INTO les_picking_ser WHERE matnr EQ <lfs_picking_det>-material.

                les_serno-material = les_picking_ser-matnr.
                les_serno-nro_serie = les_picking_ser-serie.

                APPEND les_serno TO lti_serno.
              ENDLOOP.

*.....Verifico que el Material Tenga Número de Serie
              IF lti_serno IS NOT INITIAL.
                APPEND LINES OF lti_serno TO c_seriales.
                SORT c_seriales ASCENDING.
                DELETE ADJACENT DUPLICATES FROM c_seriales COMPARING ALL FIELDS.
                les_det_pos-seriales = 'X'.
              ELSE.
                les_det_pos-seriales = ' '.
              ENDIF.
*.....Verifico si el Material Tiene Número de Lote
              IF lti_charg  IS NOT INITIAL  .
                APPEND LINES OF lti_charg TO c_lotes.
                SORT c_lotes ASCENDING.
                DELETE ADJACENT DUPLICATES FROM c_lotes COMPARING ALL FIELDS.
                les_det_pos-numero_lote = 'X'.
              ELSE.
                les_det_pos-numero_lote = ' '.
              ENDIF.
*.....Limpieza de Variables
              CLEAR : lti_detalle, lti_mensajes.
*{   REPLACE        ER3K900332                                        6
*\*.....Función para Obtener EANs de un Material
*\              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                EXPORTING
*\                  matnr      = <lfs_picking_det>-material
*\                TABLES
*\                  t_detalle  = lti_detalle
*\                  t_mensajes = lti_mensajes.
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores en los EAN´s del Material
*\              READ TABLE lti_mensajes  INDEX 1 ASSIGNING  <lfs_mensajes>.
*\              IF sy-subrc EQ 0 .
*\                les_msg-num_doc = <lfs_mensajes>-num_doc.
*\                les_msg-msg = <lfs_mensajes>-msg.
*\                les_msg-type_msg = <lfs_mensajes>-msg.
*\                APPEND les_msg TO c_mensajes.
*\              ELSE.
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\                IF lti_detalle IS NOT INITIAL.
*\                  UNASSIGN <lfs_detalle>.
*\                  LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                    les_det_pos-num_entrega = les_picking_cab-rsnum.
*\                    les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
*\                    les_det_pos-material = <lfs_picking_det>-material.
*\                    les_det_pos-desc_material = <lfs_detalle>-maktx.
*\                    les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
*\                    les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
*\                    les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
*\                    les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*\*.....Realizar la conversión de la unidad de medida
*\*                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\*                      EXPORTING
*\*                        input          = <lfs_picking_det>-umc
*\*                        language       = 'S'
*\*                      IMPORTING
*\*                        output         = l_e_meinh
*\*                      EXCEPTIONS
*\*                        unit_not_found = 1
*\*                        OTHERS         = 2.
*\*                    IF sy-subrc EQ 0.
*\*                      IF l_e_meinh EQ '004'.
*\                    les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*\*                      ELSE.
*\*                        les_det_pos-uni_med_doc = l_e_meinh.
*\*                      ENDIF.
*\*                    ELSE.
*\*                      les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*\*                    ENDIF.
*\*                      les_det_pos-seriales = <lfs_vbap>-seriales.
*\*                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*\*.....Datos de Detalle EAN
*\                    les_det_pos-ean = <lfs_detalle>-ean.
*\                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                    APPEND les_det_pos TO c_det_posiciones.
*\*                      CLEAR les_det_pos.
*\                  ENDLOOP.
*\                ELSE.
*\                  les_msg-num_doc = ' '.
*\                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_picking_det>-material
*\                  INTO l_e_desc_error RESPECTING BLANKS.
*\                  les_msg-msg = l_e_desc_error.
*\                  les_msg-type_msg = 'E'.
*\                  APPEND les_msg TO c_mensajes.
*\                ENDIF.
*\              ENDIF.
                IF i_flag_agrup is not  initial.
                        les_det_pos-num_entrega = les_picking_cab-rsnum.
*                        les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
                        les_det_pos-material = <lfs_picking_det>-material.
                        les_det_pos-desc_material = <lfs_picking_det>-material.
                        les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
                        les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
                        les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
                        les_det_pos-cant_contada =  ' '.
                        les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
                        les_det_pos-NM_REF_P = i_entrega.
*    .....Datos de Detalle EAN
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_picking_det>-material UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      les_det_pos-desc_material = <lfs_detalle>-maktx.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.

                ELSE.
*    .....Función para Obtener EANs de un Material
                  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                    EXPORTING
                      matnr      = <lfs_picking_det>-material
                    TABLES
                      t_detalle  = lti_detalle
                      t_mensajes = lti_mensajes.
*    .....Consulto Tabla de Mensajes para Verificar que no Existan Errores en los EAN´s del Material
                  READ TABLE lti_mensajes  INDEX 1 ASSIGNING  <lfs_mensajes>.
                  IF sy-subrc EQ 0 .
                    les_msg-num_doc = <lfs_mensajes>-num_doc.
                    les_msg-msg = <lfs_mensajes>-msg.
                    les_msg-type_msg = <lfs_mensajes>-msg.
                    APPEND les_msg TO c_mensajes.
                  ELSE.
*    .....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
                    IF lti_detalle IS NOT INITIAL.
                      UNASSIGN <lfs_detalle>.
                      LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                        les_det_pos-num_entrega = les_picking_cab-rsnum.
                        les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
                        les_det_pos-material = <lfs_picking_det>-material.
                        les_det_pos-desc_material = <lfs_picking_det>-material."<lfs_detalle>-maktx.
                        les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
                        les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
                        les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
                        les_det_pos-cant_contada =  ' '.
                        les_det_pos-NM_REF_P = i_entrega.
*                          les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*    .....Realizar la conversión de la unidad de medida
*                        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*                          EXPORTING
*                            input          = <lfs_picking_det>-umc
*                            language       = 'S'
*                          IMPORTING
*                            output         = l_e_meinh
*                          EXCEPTIONS
*                            unit_not_found = 1
*                            OTHERS         = 2.
*                        IF sy-subrc EQ 0.
*                          IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*                          ELSE.
*                            les_det_pos-uni_med_doc = l_e_meinh.
*                          ENDIF.
*                        ELSE.
*                          les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*                        ENDIF.
*                          les_det_pos-seriales = <lfs_vbap>-seriales.
*                          les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*    .....Datos de Detalle EAN
                        les_det_pos-ean = <lfs_detalle>-ean.
                        les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                        les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                        APPEND les_det_pos TO c_det_posiciones.

                      ENDLOOP.
                    ELSE.
                      les_msg-num_doc = ' '.
                      CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_picking_det>-material
                      INTO l_e_desc_error RESPECTING BLANKS.
                      les_msg-msg = l_e_desc_error.
                      les_msg-type_msg = 'E'.
                      APPEND les_msg TO c_mensajes.
                    ENDIF.
                  ENDIF.
                ENDIF.
                CLEAR les_det_pos.
*}   REPLACE

            ELSE.
              CONTINUE.
            ENDIF.
          ENDLOOP.
*{   INSERT         ER3K900332                                        9
          LOOP AT c_det_posiciones into les_det_pos.
              les_det_pos-desc_material = les_det_pos-material.
              les_det_pos-material = 'MATERIALES'.
              MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING material desc_material.
          ENDLOOP.
*}   INSERT
        ELSE.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'No Esta Pendiente por Packing'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'No Esta Pendiente por Packing'.
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


METHOD zif_lgtica_documento~load_document.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Setea Atributos de las Ordenees
* Autor Prog.  : Marco Suarez G
* Fecha Creac. : 13.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 13.08.2014   ER6K907163    Marco Suarez G     Creación
*-------------------------------------------------------------------------------*
*.....Metodo para Setiar Atributos de la Ordenes
  CALL METHOD me->zif_lgtica_documento~set_ordenes
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Indicador para Proceso que Genera Entrega
   c_indicador = ' '.

ENDMETHOD.


METHOD zif_lgtica_packing~get_cabecera_packing.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Entrega
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ltp_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
        aufnr TYPE resb-aufnr,
        bwart TYPE resb-bwart,
       END OF ltp_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ty_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ty_afko.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.
  DATA :
*... Estructura para las cabecera de ventas
     les_vbak TYPE ltp_vbak,
*.....Variable Estructura con Información del Documento
     les_cab_ref TYPE zesd_cab_ref,
*... Estructura para las ordenes
     les_afko TYPE ty_afko,

*.....Tabla Interna para Datos de reserva
        lti_resb TYPE TABLE OF ltp_resb,
        les_resb TYPE ltp_resb,
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking TYPE TABLE OF zmm_t_picking,
        les_picking TYPE zmm_t_picking,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing TYPE TABLE OF zmm_t_packing,
        les_packing TYPE zmm_t_packing,

*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ty_afko,

*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,
        les_kna1 TYPE ltp_kna1,
*.....Variable Estructura para Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Rango para Fechas de Creacion de Pedidos
        rl_erdat TYPE RANGE OF  vbak-erdat,
        ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Fecha Inicio
           l_e_fecha_inicio TYPE d,
           l_o_rsnum TYPE resb-rsnum,
           lv_kunnr TYPE kunnr,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_picking> TYPE zmm_t_picking,
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_resb> TYPE ltp_resb,
                 <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "PVTA
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.

      IF i_documento_origen IS NOT INITIAL AND i_cod_cliente IS INITIAL.
        MOVE i_documento_origen TO l_o_rsnum.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_o_rsnum
          IMPORTING
            output = l_o_rsnum.

*.... Consulto las ordenes

        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE rsnum EQ l_o_rsnum.

*.... Consulto la reserva
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
         INTO TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
*           KZEAR
          AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
           AND werks EQ detalles_usuario-centro.

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
        SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
         APPENDING TABLE lti_resb
         FROM resb
         WHERE rsnum EQ l_o_rsnum
           AND xloek EQ ''
           AND bwart EQ '301'
*           KZEAR
           AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
           AND umwrk EQ detalles_usuario-centro.


      ELSEIF i_cod_cliente IS NOT INITIAL.
        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.


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
        ls_erdat-sign = 'I'.
        ls_erdat-option = 'BT'.
        ls_erdat-low = l_e_fecha_inicio.
        ls_erdat-high = l_e_fecha_fin.
        APPEND ls_erdat TO rl_erdat.

*.... Consulto las ordenes en el rango de fechas
        SELECT aufnr gltrp rsnum
        INTO TABLE lti_afko
        FROM afko
        WHERE gltrp IN rl_erdat.

*.... Valido que se hayan encontrado ordenes
        IF sy-subrc EQ 0.
*.... Ordeno la tabal de ordenes
          SORT lti_afko BY rsnum aufnr.
*.... Consulto la reserva
          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
            INTO TABLE lti_resb
            FROM resb
            FOR ALL ENTRIES IN lti_afko
            WHERE rsnum EQ lti_afko-rsnum
              AND xloek EQ ''
              AND kzear EQ ''
*     Filtro dependiendo del Usuario de la consulta
              AND werks EQ detalles_usuario-centro.
*              AND lgort EQ detalles_usuario-almacen.

*.... Consulto la reserva clase mov 301 el filtro de centro es por centro receptor
          SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins aufnr bwart
           APPENDING TABLE lti_resb
           FROM resb
           FOR ALL ENTRIES IN lti_afko
           WHERE rsnum EQ lti_afko-rsnum

             AND xloek EQ ''
             AND bwart EQ '301'
*           KZEAR
             AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
             AND umwrk EQ detalles_usuario-centro.
        ENDIF.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_resb[] IS NOT  INITIAL.
*.....verificar documentos despachados para entregas
        SELECT *
           FROM zmm_t_packing
             INTO TABLE lti_packing
               FOR ALL ENTRIES IN lti_resb
                  WHERE rsnum  EQ lti_resb-rsnum AND
                        estado EQ 'CERRADO'.

*.....Busqueda de documentos Pendientes por Packing para entregas
        SELECT *
           FROM zmm_t_picking
             INTO TABLE lti_picking
               FOR ALL ENTRIES IN lti_resb
                  WHERE rsnum EQ lti_resb-rsnum AND
                        estado EQ 'CERRADO'.


*.....Verifico que Existan Registros Pendientes por Packing Campo
        IF sy-subrc NE 0.
          les_msg-msg = 'No Existen Documentos Pendientes por Packing'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ELSE.
*.....Busco Información para la Clase de Documento
          READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
          IF sy-subrc EQ 0 .

*.....Recorro Tabla de Cabeceras
            LOOP AT lti_picking ASSIGNING <lfs_picking>.
              READ TABLE lti_packing WITH KEY rsnum = <lfs_picking>-rsnum INTO les_packing.
              IF sy-subrc EQ 0.
                CONTINUE.
              ENDIF.
              READ TABLE lti_resb WITH KEY rsnum = <lfs_picking>-rsnum
                                  ASSIGNING <lfs_resb>.
              IF <lfs_resb> IS ASSIGNED.

*.....Busco Información de Cabecera del Pedido de Ventas
                SELECT SINGLE vbeln erdat auart kunnr
                     FROM vbak
                        INTO les_vbak
                          WHERE aufnr EQ les_resb-aufnr. " AND
*                              auart EQ <lfs_actividad>-cod_clase.
*.....Consulto Información del Cliente
                SELECT SINGLE kunnr name1
                     FROM  kna1
                         INTO les_kna1
                              WHERE kunnr EQ les_vbak-kunnr.
*.... Asigno el cliente a el field symbol
                ASSIGN les_kna1 TO <lfs_kna1>.
*.... Válido que haya encontrado cliente
                IF <lfs_kna1> IS ASSIGNED.
                  IF <lfs_picking>-rsnum IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-rsnum.
                  ENDIF.

                  IF <lfs_picking>-aufnr IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-aufnr.
                  ENDIF.
                  les_cab_ref-fecha_crea  = <lfs_picking>-fecha.
*            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
                  les_cab_ref-cod_cliente = les_vbak-kunnr.
                  les_cab_ref-nom_cliente = <lfs_kna1>-name1.
*                  les_cab_ref-vbeln = <lfs_picking>-vbeln2.
                  APPEND les_cab_ref TO c_det_cabecera.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ELSE.
*          Parametrización de la actividad
            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.
        ENDIF.
      ELSE.
        les_msg-msg = 'No Existen Documentos Pendientes por Packing'.
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

  ENDIF.
ENDMETHOD.


METHOD zif_lgtica_packing~get_detalle_packing.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos packing reservas
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tipo para el manejo de la reserva
  TYPES: BEGIN OF ltp_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        lgort TYPE resb-lgort,
        charg TYPE resb-charg,
        bdter TYPE resb-bdter,
        bdmng TYPE resb-bdmng,
        meins TYPE resb-meins,
        enmng TYPE resb-enmng,
        aufnr TYPE resb-aufnr,
        bwart TYPE resb-bwart,
          seriales TYPE	c LENGTH 1 ,            "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1,          "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote

       END OF ltp_resb.
*... Tipo para el manejo de los documentos y codigo de reserva
  TYPES: BEGIN OF ltp_afko,
          aufnr TYPE afko-aufnr,"Documento
          gltrp TYPE afko-gltrp,"Fecha final
          rsnum TYPE afko-rsnum,"Codigo de reserva
         END OF ltp_afko.

*.....Typo para Detalle Cabecera  de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
        aufnr TYPE aufnr,
      END OF ltp_vbak.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*.... Tipo para la descripción del material
  TYPES: BEGIN OF ltp_makt,
          matnr TYPE resb-matnr,
          maktx TYPE makt-maktx,
         END OF ltp_makt.

  DATA :
*.... Tabla interna para la descripción del material
        lti_makt TYPE TABLE OF ltp_makt,
*... Tabla interna para las ordenes
        lti_afko TYPE STANDARD TABLE OF ltp_afko,
*.....Tabla para Detalle Cabecera de Ventas
        lti_vbak TYPE TABLE OF ltp_vbak,
*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,

*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Información Detalles de Documento Picking
        lti_picking_det TYPE zttsd_picking_det,
*        les_picking_det TYPE zedsd_picking_det,
*.....Información Seriales
        lti_picking_ser TYPE zttsd_picking_ser,
        les_picking_ser TYPE zedsd_picking_ser,
*.....Información Lotes
        lti_picking_lot TYPE zttsd_picking_lot,
        les_picking_lot TYPE zedsd_picking_lot,

*..... objeto de clase picking
         lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Estructura con Datos Principales de reserva
          lti_resb TYPE STANDARD TABLE OF ltp_resb,
          les_resb TYPE ltp_resb,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum,
*.....Variable Estructura para Tabla de Mensajes
           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           les_serno  TYPE zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           les_charg  TYPE zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable para la reserva
       l_o_rsnum TYPE resb-rsnum,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

           lv_vbeln TYPE vbeln,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS :
                  <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_picking_det> TYPE zmm_t_picking_dt,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                  <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "PVTA
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido si Existe Información para el Usuario del Dispositivo
    IF detalles_usuario IS NOT INITIAL.

      MOVE i_documento_origen TO l_o_rsnum .
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_o_rsnum
        IMPORTING
          output = l_o_rsnum.

*.... Consulto las ordenes
      SELECT aufnr gltrp rsnum
      INTO TABLE lti_afko
      FROM afko
      WHERE rsnum EQ l_o_rsnum.

*.... Consulto la reserva
      SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
       INTO CORRESPONDING FIELDS OF TABLE lti_resb
       FROM resb
       WHERE rsnum EQ l_o_rsnum
         AND xloek EQ ''
*           KZEAR
        AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
         AND werks EQ detalles_usuario-centro.

*.... Consulto la reserva
      SELECT rsnum rspos matnr werks lgort charg bdter bdmng meins enmng aufnr bwart
       APPENDING CORRESPONDING FIELDS OF TABLE lti_resb
       FROM resb
       WHERE rsnum EQ l_o_rsnum
         AND xloek EQ ''
*           KZEAR
        AND kzear EQ ''
*      Filtro dependiendo del Usuario de la consulta
         AND umwrk EQ detalles_usuario-centro.
*           AND lgort EQ detalles_usuario-almacen.
      IF sy-subrc EQ 0.
*.... Consulto la descripción de los materiales
        SELECT matnr maktx
          INTO TABLE lti_makt
          FROM makt
          FOR ALL ENTRIES IN lti_resb
          WHERE matnr EQ lti_resb-matnr.
      ENDIF.

*.....Verifico que Existan Registros
      IF sy-subrc EQ 0 .
*.... Obtengo el picknum de la reserva
        CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
          EXPORTING
            i_rsnum   = l_o_rsnum
          RECEIVING
            e_picknum = l_e_piknum.


      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.

*.....Verifico que exista picknum a Procesar
      IF l_e_piknum IS NOT INITIAL.
*.....Creo Instancia de Clase
        IF  lo_lgtica_picking IS NOT BOUND  .
          CREATE OBJECT lo_lgtica_picking .
        ENDIF.
*.....Cargo Atributos Picking
        lo_lgtica_picking->load_picking(
          EXPORTING
            i_picknum = l_e_piknum ).

*.....Busco el Codigo de la Clase para El documento Recibido
        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc NE 0 .
          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
*.....Inicia Proceso de Actualización de reserva
*.....Consulto Cabecera Documento Picking
        les_picking_cab = lo_lgtica_picking->s_picking.

        IF  les_picking_cab-estado EQ 'CERRADO' AND
            les_picking_cab IS NOT INITIAL.


*.....Consulto detalle Documento Picking
          lti_picking_det = lo_lgtica_picking->t_picking_det.
*.....Consulto seriales Documento Picking
          lti_picking_ser = lo_lgtica_picking->t_picking_ser.
*.....Consulto lotes Documento Picking
          lti_picking_lot = lo_lgtica_picking->t_picking_lot.
*.... Filtro seriales y lotes de la reserva
*          DELETE lti_picking_lot WHERE vbeln2 NE lv_vbeln.
*          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.

*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det>.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = <lfs_picking_det>-material.

            les_mara-matnr = <lfs_picking_det>-material.
            APPEND les_mara TO lti_mara.

          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_mara = lti_mara.
*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
*.....Función para Ajuste de Número
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lfs_picking_det>-material
              IMPORTING
                output = <lfs_picking_det>-material.

*....    Valido material pertenezca a la reserva ( Centro y Almacen )
            READ TABLE lti_resb INTO les_resb WITH KEY  matnr = <lfs_picking_det>-material.
            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_picking_det>-material.
              APPEND l_e_matnr TO lti_matnr.

*.....Verifico si el Material Tiene Número de Lote

              LOOP AT lti_picking_lot  INTO les_picking_lot WHERE matnr EQ <lfs_picking_det>-material.

                les_charg-matnr = <lfs_picking_det>-material.
*.....Limpieza de Variables
                CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                  EXPORTING
                    matnr      = <lfs_picking_det>-material
                  TABLES
                    t_detalle  = lti_detalle
                    t_mensajes = lti_mensajes.
                UNASSIGN <lfs_detalle>.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
                READ TABLE lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = les_picking_lot-umc.
                IF sy-subrc EQ 0.
                  les_charg-clabs = les_picking_lot-cantidad * <lfs_detalle>-cantidad_ean.
                ELSE.
                  les_charg-clabs = les_picking_lot-cantidad.
                ENDIF.
                les_charg-charg = les_picking_lot-lote.

                APPEND les_charg TO lti_charg.
              ENDLOOP.

*.....Verifico si el Material Tiene Número de serie

              LOOP AT lti_picking_ser  INTO les_picking_ser WHERE matnr EQ <lfs_picking_det>-material.

                les_serno-material = les_picking_ser-matnr.
                les_serno-nro_serie = les_picking_ser-serie.

                APPEND les_serno TO lti_serno.
              ENDLOOP.

*.....Verifico que el Material Tenga Número de Serie
              IF lti_serno IS NOT INITIAL.
                APPEND LINES OF lti_serno TO c_seriales.
                SORT c_seriales ASCENDING.
                DELETE ADJACENT DUPLICATES FROM c_seriales COMPARING ALL FIELDS.
                les_det_pos-seriales = 'X'.
              ELSE.
                les_det_pos-seriales = ' '.
              ENDIF.
*.....Verifico si el Material Tiene Número de Lote
              IF lti_charg  IS NOT INITIAL  .
                APPEND LINES OF lti_charg TO c_lotes.
                SORT c_lotes ASCENDING.
                DELETE ADJACENT DUPLICATES FROM c_lotes COMPARING ALL FIELDS.
                les_det_pos-numero_lote = 'X'.
              ELSE.
                les_det_pos-numero_lote = ' '.
              ENDIF.
*.....Limpieza de Variables
              CLEAR : lti_detalle, lti_mensajes.
*.....Función para Obtener EANs de un Material
              CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                EXPORTING
                  matnr      = <lfs_picking_det>-material
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
                  UNASSIGN <lfs_detalle>.
                  LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                    les_det_pos-num_entrega = les_picking_cab-rsnum.
                    les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
                    les_det_pos-material = <lfs_picking_det>-material.
                    les_det_pos-desc_material = <lfs_detalle>-maktx.
                    les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
                    les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
                    les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
                    les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
*                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*                      EXPORTING
*                        input          = <lfs_picking_det>-umc
*                        language       = 'S'
*                      IMPORTING
*                        output         = l_e_meinh
*                      EXCEPTIONS
*                        unit_not_found = 1
*                        OTHERS         = 2.
*                    IF sy-subrc EQ 0.
*                      IF l_e_meinh EQ '004'.
                    les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*                      ELSE.
*                        les_det_pos-uni_med_doc = l_e_meinh.
*                      ENDIF.
*                    ELSE.
*                      les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
*                    ENDIF.
*                      les_det_pos-seriales = <lfs_vbap>-seriales.
*                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
                    les_det_pos-ean = <lfs_detalle>-ean.
                    les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                    les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                    APPEND les_det_pos TO c_det_posiciones.
*                      CLEAR les_det_pos.
                  ENDLOOP.
                ELSE.
                  les_msg-num_doc = ' '.
                  CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_picking_det>-material
                  INTO l_e_desc_error RESPECTING BLANKS.
                  les_msg-msg = l_e_desc_error.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDIF.

            ELSE.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ELSE.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'No Esta Pendiente por Packing'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'No Esta Pendiente por Packing'.
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


method ZIF_LGTICA_PACKING~GET_HEADER_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Genara Estructura de Cabecera para un documento de pAcking
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.... Estructura para el manejo del pAcking
  DATA: les_header_pAcking TYPE zedsd_pAcking.
*.... Estructuras para el manejo de las entregas
  DATA: lti_entregas TYPE STANDARD TABLE OF zesd_det_ent_pkg,
        les_entregas TYPE zesd_det_ent_pkg,
*.... Estructuras para el manejo de las reservas
        lti_reservas type STANDARD TABLE OF ZESD_CABECERA_RESERVAS,
        les_reservas type ZESD_CABECERA_RESERVAS,
*.....Variable de Ref a la Clase Picknum
        lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum.


*.... Copio el atributo de las reservas a la estructura
  MOVE CABECERA_RESERVAS TO les_reservas.
*.... Ordeno la tabla y eliminimo repetidos
*  sorT lti_reservas BY rsnum.

*  DELETE ADJACENT DUPLICATES FROM lti_reservas COMPARING rsnum.

*  READ TABLE lti_reservas INTO les_reservas INDEX 1.

*.....Consulto el Pikum de la reserva
  CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
    EXPORTING
      i_rsnum = les_reservas-rsnum
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

*.....Consulto Cabecera Documento Picking
  les_picking_cab = lo_lgtica_picking->s_picking.

 IF les_picking_cab-estado EQ 'CERRADO'.
    les_header_packing-mandt = sy-mandt.
    les_header_packing-PROCESO = 'PACKING'.
*    les_header_packing-SUBPROCESO = les_picking_cab
    les_header_packing-VBELN = les_picking_cab-VBELN.
    les_header_packing-EBELN = les_picking_cab-EBELN.
    les_header_packing-AGLUTINADOR = les_picking_cab-AGLUTINADOR.
    les_header_packing-VBELN2 = les_picking_cab-VBELN2.
    les_header_packing-AUFNR = les_picking_cab-AUFNR.
    les_header_packing-RSNUM = les_picking_cab-RSNUM.
    les_header_packing-fecha = sy-datum.
    les_header_packing-hora = sy-uzeit.
    les_header_packing-usuario = detalles_usuario-usuario.
    les_header_packing-uname = sy-uname.
    les_header_packing-femod = sy-datum.
    les_header_packing-homod = sy-uzeit.
    les_header_packing-usrmod = detalles_usuario-usuario.
    les_header_packing-unamod = sy-uname.

*   MOVE les_picking_cab to les_header_pAcking.
    APPEND les_header_packing TO r_header_packing.

 ENDIF.

endmethod.


method ZIF_LGTICA_PACKING~GET_POS_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Genara Estructura de detalle para un documento de packing
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*

*.... Estructura para el manejo del packing detalle
  data: les_pos_packing type ZEDSD_PaCKING_pos.
*.... Estructuras para el manejo de las entregas
  DATA: lti_entregas TYPE STANDARD TABLE OF zesd_det_ent_pkg,
        les_entregas TYPE zesd_det_ent_pkg,
*.... Estructuras para el manejo de las reservas
        lti_reservas type STANDARD TABLE OF ZESD_CABECERA_RESERVAS,
        les_reservas type ZESD_CABECERA_RESERVAS,
*.....Variable de Ref a la Clase Picknum
        lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Información Detalles de Documento Picking
        lti_picking_det TYPE zttsd_picking_det,
        les_picking_det TYPE zedsd_picking_det,
*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Información Seriales
        lti_picking_ser TYPE zttsd_picking_ser,
        les_picking_ser TYPE zedsd_picking_ser,
*.....Información Lotes
        lti_picking_lotes TYPE zttsd_picking_lot,
        les_picking_lotes TYPE zedsd_picking_lot,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum.

*.... Copio el atributo de las reservas a la estructura
  MOVE CABECERA_RESERVAS TO les_reservas.
*.... Ordeno la tabla y eliminimo repetidos
*  sorT lti_reservas BY rsnum.

*  DELETE ADJACENT DUPLICATES FROM lti_reservas COMPARING rsnum.

*  READ TABLE lti_reservas INTO les_reservas INDEX 1.

*.....Consulto el Pikum de la reserva
  CALL METHOD zcl_lgtica_picking=>get_data_by_rsnum
    EXPORTING
      i_rsnum = les_reservas-rsnum
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

*.....Consulto Detalle Documento Picking para llenar tabla de Función
    lti_picking_det = lo_lgtica_picking->t_picking_det.
*.....Consulto Seriales
    lti_picking_ser = lo_lgtica_picking->t_picking_ser.
*.....Consulto Lotes
    lti_picking_lotes = lo_lgtica_picking->t_picking_lot.

*.... Recorro el detalle de picking
    LOOP AT lti_picking_det into les_picking_det where p_confirm eq 'CT'.

          les_pos_packing-POSICION = les_picking_det-posicion.
*{   INSERT         ER3K900332                                        1
          les_pos_packing-pospack  = les_picking_det-pospick          .
*}   INSERT

          les_pos_packing-MATERIAL = les_picking_det-material.
          les_pos_packing-CANTIDAD = les_picking_det-cantidad.
          les_pos_packing-UMC = les_picking_det-umc. " unidad de medida
*          les_pos_packing-UMC = les_lips-MEINS. " unidad de medida base
          les_pos_packing-DIFERENCIA = les_pos_packing-CANTIDAD.
          les_pos_packing-UMD = les_pos_packing-UMC.
          les_pos_packing-UBICACION_TMP = les_picking_det-UBICACION_TMP.
          les_pos_packing-UBICACION_FI = les_picking_det-UBICACION_FI.
          les_pos_packing-FECHA = sy-datum.
          les_pos_packing-HORA = sy-uzeit.
          les_pos_packing-DOC_ASOCIADO = les_picking_det-DOC_ASOCIADO.
          les_pos_packing-VBELN2 = les_picking_det-VBELN2.
          les_pos_packing-RSNUM = les_picking_det-RSNUM.
          les_pos_packing-AUFNR = les_picking_det-AUFNR.

          les_pos_packing-VBELN2 = les_pos_packing-vbeln2.
*.... Adicionar el registro a la estructura para la generación de los datos de packing
          APPEND les_pos_packing to r_pos_packing.
    endloop.


endmethod.


method ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING.
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
* Reservas
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de reservas
  DATA: les_reservas TYPE ZESD_CABECERA_RESERVAS.
*.... Estructura para el manejo del picking
  DATA: les_header_picking TYPE zedsd_picking.

*.... Leo la cabecera de reservas
  IF CABECERA_RESERVAS is not initial.

    les_header_picking-mandt = sy-mandt.
*les_header_picking-PICKNUM =
*les_header_picking-PROCESO =
*les_header_picking-SUBPROCESO =
    les_header_picking-rsnum = CABECERA_RESERVAS-rsnum.
    les_header_picking-aufnr = CABECERA_RESERVAS-aufnr.
*    les_header_picking-ebeln = les_ventas-ebeln.
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

      APPEND les_header_picking TO r_header_picking.

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
* RESERVAS
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de detalle de reservas
  data: les_reservas_dt type BAPI2093_RES_ITEMS_GET.
**.... Estructura para el manejo de la cabecera de reservas
* data: les_reservas_dt type ZESD_CABECERA_RESERVAS.

*.... Estructura para el manejo del picking detalle
  data: les_pos_picking type ZEDSD_PICKING_DET.
*.... recorro el atributo de detalle de reservas
  LOOP AT DETALLES_RESERVAS into les_reservas_dt.
    clear:les_pos_picking.
**
*          READ table CABECERA_RESERVAS into les_reservas with key rsnum  = les_reservas_dt-RESERV_NO.
*          IF sy-subrc eq 0.
*
*          ENDIF.
          les_pos_picking-MATERIAL = les_reservas_dt-material.
          les_pos_picking-CANTIDAD = les_reservas_dt-ENTRY_QNT.
          les_pos_picking-posicion = les_reservas_dt-RES_ITEM.
          les_pos_picking-UMC = les_reservas_dt-ENTRY_UOM. " unidad de medida venta
*          les_pos_picking-UMC = les_lips-MEINS. " unidad de medida base
*          les_pos_picking-DIFERENCIA
*          les_pos_picking-UMD
*          les_pos_picking-UBICACION_TMP
*          les_pos_picking-UBICACION_SU
          les_pos_picking-FECHA = sy-datum.
          les_pos_picking-HORA = sy-uzeit.
*          les_pos_picking-CONTEO
          les_pos_picking-CANTCONT = les_reservas_dt-WITHD_QUAN.
          les_pos_picking-UMCC = les_reservas_dt-BASE_UOM.
*          les_pos_picking-UMCC
*          les_pos_picking-P_CONFIRM
*          les_pos_picking-DOC_ASOCIADO
          les_pos_picking-rsnum = les_reservas_dt-RESERV_NO.
          append les_pos_picking to r_pos_picking.
  ENDLOOP.
endmethod.
ENDCLASS.
