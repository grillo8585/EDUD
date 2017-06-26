class ZCL_LGTICA_ORD_SERV_EXT definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_ORD_SERV_EXT
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
*"* protected components of class ZCL_LGTICA_PROYECTOS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_PROYECTOS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_ORD_SERV_EXT IMPLEMENTATION.


METHOD zif_lgtica_documento~generar_entregas.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Entregas para Proceso ordenes de servicio Externas
* Autor Prog.  :
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*

*.....Typo para Procesamiento de Errores al Generar la Entrega
*{   REPLACE        ER3K900279                                        1
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
          les_bapiret1 TYPE bapiret1,
*.....Tabla Interna con Datos Necesarios para Generación de Entrega
          lti_key_enque_read TYPE shp_vl10_package_t,
*.....Estructura para Generación de Entrega
          les_key_enque_read TYPE shp_vl10_package,
*.....Tabla Interna con Entregas
          lti_vbls TYPE shp_vbls_t,
*.....Tabla Interna con Proceso de Errores
         lti_vbsk TYPE shp_vbsk_t,
*.....Variable con Entrega
          les_entregas TYPE zesd_detalles_entregas,
*.....Estructuras para Mensajes
          les_msg TYPE zesd_msg_picking,
*.....Variable de Mensajes
          l_e_msg TYPE string,
*.....Variable Fecha
          l_e_fecha TYPE sy-datum,
*{   INSERT         ER3K900279                                        2
          e_syst type syst,

          number type SYMSGNO,
*}   INSERT

          lo_pick TYPE REF TO zif_lgtica_pickable,
          lo_pick_clas TYPE REF TO zcl_lgtica_picking.

*.....Field Simbols para Procesamiento de Información de Compras
  FIELD-SYMBOLS: <lfs_vbap> TYPE zesd_posiciones_ventas,
*{   REPLACE        ER3K900279                                        9
*\                 <lfs_vbak> TYPE vbak,
                 <lfs_vbak> TYPE ZESD_CAB_PED,
*}   REPLACE
                 <lfs_vbls> TYPE vbls.

*.....Metodo para Setiar Atributos
*  CALL METHOD me->zif_lgtica_documento~set_ventas
*    EXPORTING
*      i_clase_documento  = i_clase_documento
*      i_documento_origen = i_documento_origen.

*.....Verifico que no Existan Errores en la Información Correspondiente al Usuario
  IF detalles_usuario IS NOT INITIAL  .
    IF detalles_ventas[] IS NOT INITIAL.
*.....Verifico el Número de Entregas a Generar
      LOOP AT detalles_ventas  ASSIGNING <lfs_vbap>.
*.....Para el Centro Generar la Entrega
        AT END OF werks.
          l_e_fecha = sy-datum.

          CLEAR :les_key_enque_read, lti_key_enque_read, les_entregas, lti_vbls.

*.....Busco la Fecha de Creacíon del Documento
          READ TABLE cabeceras_ventas WITH KEY vbeln = <lfs_vbap>-vbeln
                                        ASSIGNING <lfs_vbak>.

          IF sy-subrc EQ 0 .
            les_key_enque_read-panum = '1'.
            les_key_enque_read-vbobj = 'A'.
            les_key_enque_read-vbtyp = 'C'.
            les_key_enque_read-vbeln = <lfs_vbak>-vbeln.
            les_key_enque_read-id = '1'.
            les_key_enque_read-ledat =  l_e_fecha. "<lfs_vbak>-erdat.
            les_key_enque_read-tabix = '1'.
            les_key_enque_read-vstel = <lfs_vbap>-vstel.
            les_key_enque_read-kunwe = <lfs_vbak>-kunnr.
            APPEND les_key_enque_read TO lti_key_enque_read.

            CALL METHOD me->call_bapi_delivery1
              EXPORTING
                i_ledat           = l_e_fecha "<lfs_vbak>-erdat
                i_nur_vorgabe_pos = ' '
                i_key_enque_read  = lti_key_enque_read
              IMPORTING
                e_vbls            = lti_vbls
*{   REPLACE        ER3K900279                                        3
*\                e_vbsk            = lti_vbsk.
                e_vbsk            = lti_vbsk
                e_syst            = e_syst.
*}   REPLACE
            IF lti_vbls[] IS NOT INITIAL.
*.....Ordeno Tabla
              SORT lti_vbls BY vbeln_lif posnr_lif  ASCENDING.
*.....Elimino Registros Duplicados
*            DELETE ADJACENT DUPLICATES FROM  lti_vbls  COMPARING  vbeln_lif posnr_lif.

**.....Actualizo Base de Datos
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
              LOOP AT lti_vbls ASSIGNING <lfs_vbls> .
                les_entregas-documento =  <lfs_vbak>-vbeln.
                les_entregas-entrega =  <lfs_vbls>-vbeln_lif.
                APPEND les_entregas TO c_entregas.
                CLEAR : les_entregas.
              ENDLOOP.
            ELSE.
*.....Proceso de Manejo de Errores
              IF lti_vbsk IS NOT INITIAL .
*{   REPLACE        ER3K900279                                        4
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
*{   REPLACE        ER3K900279                                        5
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

                    CONCATENATE 'Entrega para el Doc ' space <lfs_vbak>-vbeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_vbak>-vbeln.
                    les_msg-msg = l_e_msg.
*{   REPLACE        ER3K900279                                        6
*\                    les_msg-type_msg = les_bapiret1-type.
                    les_msg-type_msg = 'E'.
*}   REPLACE
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
                  ENDLOOP.
                  CLEAR : lti_vbfs.
*{   INSERT         ER3K900279                                        7
                ELSE.
                  number =  e_syst-msgno.
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

                      CONCATENATE 'Entrega para el Doc ' space <lfs_vbak>-vbeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_vbak>-vbeln.
                    les_msg-msg = l_e_msg.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
*}   INSERT
                ENDIF.
              ELSE.
*{   REPLACE        ER3K900279                                        8
*\                CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_vbak>-vbeln space
*\                          ' Centro Nro' space <lfs_vbap>-werks space  'Verifique Información'
*\                           INTO l_e_msg RESPECTING BLANKS.
*\                les_msg-num_doc = <lfs_vbak>-vbeln.
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

                      CONCATENATE 'Entrega para el Doc ' space <lfs_vbak>-vbeln space
                                 les_bapiret1-message space INTO l_e_msg RESPECTING BLANKS.

                    les_msg-num_doc = <lfs_vbak>-vbeln.
                    les_msg-msg = l_e_msg.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    CLEAR : les_msg, les_vbfs, les_bapiret1.
*}   REPLACE
              ENDIF.
            ENDIF.
          ENDIF.
        ENDAT.
      ENDLOOP.
*.....Elimino Registros Duplicados
      DELETE ADJACENT DUPLICATES FROM c_entregas COMPARING documento entrega.
*                me->entregas = c_entregas.
      me->zif_lgtica_documento~t_entregas = c_entregas.
      lo_pick = me.
*....
      CALL METHOD zcl_lgtica_picking=>create_by_pkble_ref
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


METHOD ZIF_LGTICA_DOCUMENTO~GET_CABECERA.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Ordenes de Servicio en Externas
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*

*.....Typo para Tabla VBAP
  TYPES : BEGIN OF ltp_vbap,
*{   REPLACE        ER3K900279                                        1
*\           vbeln TYPE vbeln_va,    "Documento de ventas
*\           posnr TYPE posnr_va,    "Posición documento ventas
*\           werks TYPE werks_ext,   "Centro (propio o externo)
*\           lgort TYPE lgort_d,
          vbeln TYPE 	ebeln,   "VBAP
          posnr	 TYPE posnr,   "VBAP
          matnr TYPE matnr,    "VBAP
          arktx TYPE arktx,    "VBAP
          kwmeng TYPE kwmeng,   "VBAP
          vrkme TYPE vrkme,            "VBAP
          UMVKZ type UMVKZ ,
          UMVKN type UMVKN,
          meins type meins,
          werks TYPE werks_ext,        "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLO
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          auart TYPE auart,            "EKKO Clase de documento de Ventas
*}   REPLACE
         END OF ltp_vbap.

*.....Typo para Tabla VBUP, Pedidos de Venta Pendiente por Picking
  TYPES : BEGIN OF ltp_vbup,
          vbeln TYPE vbeln,       "Número de documento comercial
          posnr TYPE posnr,       "Número de posición del documento comercial
          lfgsa TYPE 	lfgsa,      "Status total de entrega de posición
    END OF ltp_vbup.

*.....Typo para Detalle Cabecera Pedido de Ventas VBAK
  TYPES : BEGIN OF ltp_vbak,
        vbeln TYPE vbeln_va,   "Documento de ventas
        erdat TYPE erdat,      "Fecha de creación del registro
        auart TYPE auart,      "Clase de documento de ventas
        kunnr TYPE kunag,      "Solicitante
      END OF ltp_vbak.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
*{   INSERT         ER3K900279                                        2

*Tabla interna de ubicación
  DATA: lti_ubicacion TYPE STANDARD TABLE OF zmm_t_ubicacion.
*... Tabla interna para consultar la tabla de ubicaciones
data: ti_zmm_t_ubicacion type standard table of zmm_t_ubicacion.
*...
data: les_ubicacion type  zmm_t_ubicacion.

data: les_ubicacion_suge type  ZESD_UBIC_SUG.

  DATA :
*.....Tabla Interna para Busqueda de Pedidos de Venta Pendientes por Picking
*           lti_vbup TYPE TABLE OF ltp_vbup,
*.....Tabla Interna de Posiciones de Pedidos de Venta
*           lti_vbap TYPE TABLE OF ltp_vbap,
*.....Tabla Interna para Cabecera Pedidos de Venta
*           lti_vbak TYPE TABLE OF ltp_vbak,
*.....Variable Estructura para Tabla de Mensajes
*           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*           lv_vbeln type vbeln,
*.....Tabla Internas de Retorno al Consultar la Actividad
*        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS :
*                  <lfs_vbup> TYPE ltp_vbup,
                  <lfs_vbap> TYPE ltp_vbap,
*                  <lfs_vbak> TYPE ltp_vbak,
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
*.....Tabla Interna del Tipo ltp_vbap
        lti_vbap TYPE TABLE OF ltp_vbap,
*.....Tabla Interna del Tipo ltp_vbup
        lti_vbup TYPE TABLE OF ltp_vbup,
*.....Tabla para Detalle Cabecera Pedido de Ventas
        lti_vbak TYPE TABLE OF ltp_vbak,
        les_vbak TYPE ltp_vbak,
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

        lv_vbeln type vbeln,
*.....Fecha Inicio
           l_e_fecha_inicio TYPE d,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_vbup> TYPE ltp_vbup,
                 <lfs_vbak> TYPE ltp_vbak,
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_actividad> TYPE zmm_t_clase_pv.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "PROY
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

        move i_documento_origen to lv_vbeln.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_vbeln
    IMPORTING
      output = lv_vbeln.


*.....Busqueda de Pedidos de Venta para El Usuario Según el Centro, Almacen y Documento Origne que tengan Asignados
*{   REPLACE        ER3K900279                                        3
*\        SELECT vbeln posnr werks lgort
      SELECT vbeln posnr matnr arktx kwmeng vrkme UMVKZ UMVKN meins werks lgort
*}   REPLACE
                   FROM  vbap
                    INTO TABLE lti_vbap
                      WHERE werks EQ detalles_usuario-centro AND
                            lgort EQ detalles_usuario-almacen AND
                            vbeln EQ lv_vbeln.
      ELSE.
*.....Busqueda de Pedidos de Venta para El Usuario Según el Centro y Almacen que tengan Asignados
*{   REPLACE        ER3K900279                                        4
*\        SELECT vbeln posnr werks lgort
      SELECT vbeln posnr matnr arktx kwmeng vrkme UMVKZ UMVKN meins werks lgort
*}   REPLACE
               FROM  vbap
                INTO TABLE lti_vbap
                  WHERE werks EQ detalles_usuario-centro AND
                        lgort EQ detalles_usuario-almacen.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_vbap[] IS NOT  INITIAL.
*.....Ordeno Tabla para el Proceso en el For all Entries
        SORT lti_vbap BY vbeln posnr ASCENDING.
*.....Busqueda de documentos Pendientes por Picking para Pedidos de Venta
        SELECT vbeln posnr lfgsa
           FROM vbup
             INTO TABLE lti_vbup
               FOR ALL ENTRIES IN lti_vbap
                  WHERE vbeln EQ lti_vbap-vbeln AND
                        posnr EQ lti_vbap-posnr AND
*                        lfgsa ne 'C'.
                        gbsta NE 'B'.
*.....Verifico que Existan Registros Pendientes por Picking Campo lfgsa NE 'C'
        IF sy-subrc NE 0.
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

*.....Verifico Parametros de Entrada de la Función
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
        ls_erdat-sign = 'I'.
        ls_erdat-option = 'BT'.
        ls_erdat-low = l_e_fecha_inicio.
        ls_erdat-high = l_e_fecha_fin.
        APPEND ls_erdat TO rl_erdat.

*.....Ordeno Tabla para un Mejor Procesamiento del For All Entries
        SORT lti_vbup BY vbeln posnr ASCENDING.

*.....Busco Información para la Clase de Documento
        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc EQ 0 .
*.....Busco Información del Pedido de Ventas
          SELECT vbeln erdat auart kunnr
            FROM vbak
               INTO TABLE lti_vbak
                 FOR ALL ENTRIES IN lti_vbup
                   WHERE vbeln EQ lti_vbup-vbeln AND
                         erdat IN rl_erdat AND
                         auart EQ <lfs_actividad>-cod_clase.
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
*.....Verifico que Existan Registro
        IF sy-subrc EQ 0.
*......Ordeno Tabla para un Mejor Procesamiento en el For All Entries
          SORT lti_vbak BY vbeln erdat kunnr ASCENDING.

*.....Consulto Informacion del Cliente
          SELECT kunnr name1
                FROM  kna1
                 INTO TABLE lti_kna1
                    FOR ALL ENTRIES IN lti_vbak
                        WHERE kunnr EQ lti_vbak-kunnr.
        ENDIF.
*.....Recorro Tabla de Cabeceras
        LOOP AT lti_vbak ASSIGNING <lfs_vbak>.
          READ TABLE lti_kna1 WITH KEY kunnr = <lfs_vbak>-kunnr
                              ASSIGNING <lfs_kna1>.
          IF sy-subrc EQ 0.
            les_cab_ref-num_doc     = <lfs_vbak>-vbeln.
            les_cab_ref-fecha_crea  = <lfs_vbak>-erdat.
            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
            les_cab_ref-cod_cliente = <lfs_vbak>-kunnr.
            les_cab_ref-nom_cliente = <lfs_kna1>-name1.
            APPEND les_cab_ref TO c_det_cabecera.
          ENDIF.
        ENDLOOP.
*.....Si el Parametro Recibido por la Función es el Número de Pedido de Ventas
      ELSEIF i_documento_origen IS NOT INITIAL.
*.....Busco que el Número de Pedido de Ventas este Pendiente por Picking
        READ TABLE lti_vbup WITH KEY vbeln =  lv_vbeln "i_documento_origen
                            ASSIGNING <lfs_vbup>.

        IF sy-subrc EQ 0.
*.....Busco Información de Cabecera del Pedido de Ventas
          SELECT SINGLE vbeln erdat auart kunnr
               FROM vbak
                  INTO les_vbak
                    WHERE vbeln EQ <lfs_vbup>-vbeln. " AND
*                              auart EQ <lfs_actividad>-cod_clase.
*.....Consulto Información del Cliente
          SELECT SINGLE kunnr name1
               FROM  kna1
                   INTO les_kna1
                        WHERE kunnr EQ les_vbak-kunnr.

          les_cab_ref-num_doc     = les_vbak-vbeln.
          les_cab_ref-fecha_crea  = les_vbak-erdat.
          les_cab_ref-tipo_doc    = les_vbak-auart.
          les_cab_ref-cod_cliente = les_vbak-kunnr.
          les_cab_ref-nom_cliente = les_kna1-name1.
          APPEND les_cab_ref TO c_det_cabecera.
*{   INSERT         ER3K900279                                        5
          IF I_FLAG_DET EQ 'X'.
         IF lti_vbap is not INITIAL.
*   Extraemos las ubicaciones, cantidades y unidades de medida del material
*   y lo llevamos a la tabla que se exportará
    SELECT material ubicacion UBIC_DEFAULT cantidad unidad_med
    INTO CORRESPONDING FIELDS OF TABLE lti_ubicacion
    FROM zmm_t_ubicacion
    FOR ALL ENTRIES IN lti_vbap
    WHERE centro   EQ lti_vbap-werks
      AND almacen  EQ lti_vbap-lgort
      AND material EQ lti_vbap-MATnr.


ENDIF.


*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_vbap ASSIGNING <lfs_vbap> WHERE  vbeln = les_vbak-vbeln.
            les_mara-matnr = <lfs_vbap>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = c_eans
              t_mara = lti_mara.


*.....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------

  SORT lti_vbap by VBELN MATNR ASCENDING.

          LOOP AT lti_vbap ASSIGNING <lfs_vbap> WHERE  vbeln = les_vbak-vbeln.

            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_vbap>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_vbap>-werks
                  lgort     = <lfs_vbap>-lgort
                  username  = i_usuario
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
                  <lfs_vbap>-seriales = 'X'.
                ELSE.
                  <lfs_vbap>-seriales = ' '.
                ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                IF lti_charg  IS NOT INITIAL  .
                  APPEND LINES OF lti_charg TO c_lotes.
                  <lfs_vbap>-numero_lote = 'X'.
                ELSE.
                  <lfs_vbap>-numero_lote = ' '.
                ENDIF.
*                ENDIF.

IF I_FLAG_AGRUP is not initial.
  CLEAR les_det_pos.
                    les_det_pos-num_entrega = <lfs_vbap>-vbeln.
*                      les_det_pos-pos_entrega = <lfs_vbap>-posnr.
                      les_det_pos-material = <lfs_vbap>-matnr.
                      les_det_pos-desc_material = <lfs_vbap>-arktx.
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
*.... Convierto a unidad de medida base
                      IF <lfs_vbap>-VRKME NE <lfs_vbap>-MEINS.
                          <lfs_vbap>-kwmeng = ( <lfs_vbap>-kwmeng * <lfs_vbap>-UMVKZ ) / <lfs_vbap>-UMVKN.
                      ENDIF.
                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                        EXPORTING
                          input          = <lfs_vbap>-MEINS
                          language       = 'S'
                        IMPORTING
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_vbap>-MEINS.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
                      ENDIF.
                      les_det_pos-seriales = <lfs_vbap>-seriales.
                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_vbap>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
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
                    matnr      = <lfs_vbap>-matnr
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
                      les_det_pos-num_entrega = <lfs_vbap>-vbeln.
                      les_det_pos-pos_entrega = <lfs_vbap>-posnr.
                      les_det_pos-material = <lfs_vbap>-matnr.
                      les_det_pos-desc_material = <lfs_vbap>-arktx.
                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
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
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                        EXPORTING
                          input          = <lfs_vbap>-vrkme
                          language       = 'S'
                        IMPORTING
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
                      ENDIF.
                      les_det_pos-seriales = <lfs_vbap>-seriales.
                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      APPEND les_det_pos TO c_det_posiciones.
                      CLEAR les_det_pos.
                    ENDLOOP.
                  ELSE.
                    les_msg-num_doc = ' '.
                    CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_vbap>-matnr
                    INTO l_e_desc_error RESPECTING BLANKS.
                    les_msg-msg = l_e_desc_error.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                  ENDIF.
                ENDIF.
                ENDIF.

              ELSE.
                les_msg-num_doc = <lfs_vbap>-vbeln.
                les_msg-msg = <lfs_return>-message.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDLOOP.
          ENDIF.
*}   INSERT
*.....Completo Información de Respuesta
        ELSE.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'No Esta Pendiente por Picking'.
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
      c_mensajes[] = mensajes[].
      RETURN.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD ZIF_LGTICA_DOCUMENTO~GET_DETALLE.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos Ordenes de Servicio en Externa
* Autor Prog.  :
* Fecha Creac. :
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Typo para Busqueda Documento de Ventas Pendiente por Picking VBUP
  TYPES : BEGIN OF ltp_vbup,
            vbeln TYPE vbeln,       "Número de documento comercial
            posnr TYPE posnr,       "Número de posición del documento comercial
            lfgsa TYPE 	lfgsa,      "Status total de entrega de posición
      END OF ltp_vbup.

*.....Typo para Busqueda Detalle Cabecera Documento VBAK
  TYPES: BEGIN OF ltp_vbak,
         vbeln TYPE vbeln_va,       "Documento de ventas
         auart TYPE auart,          "Clase de documento de ventas
  END OF ltp_vbak.

*{   REPLACE        ER3K900304                                        1
*\*.....Typo para Busqueda Detalle Posiciones del Documento Pedido de Ventas VBAP
*\  TYPES : BEGIN OF ltp_vbap,
*\          vbeln TYPE 	ebeln,   "VBAP
*\          posnr	 TYPE posnr,   "VBAP
*\          matnr TYPE matnr,    "VBAP
*\          arktx TYPE arktx,    "VBAP
*\          kwmeng TYPE kwmeng,   "VBAP
*\          ubic_fija TYPE ze_ubicacion, "vacio
*\          ubic_tmp TYPE	ze_ubicacion,  "vacio
*\          cant_contada TYPE	menge_d,   "vacio
*\          vrkme TYPE vrkme,            "VBAP
*\          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
*\          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
*\          werks TYPE werks_ext,        "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
*\          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLOT
*\          auart TYPE auart,            "EKKO Clase de documento de Ventas
*\    END OF ltp_vbap.
*.....Typo para Busqueda Detalle Posiciones del Documento Pedido de Ventas VBAP
  TYPES : BEGIN OF ltp_vbap,
          vbeln TYPE 	ebeln,   "VBAP
          posnr	 TYPE posnr,   "VBAP
          matnr TYPE matnr,    "VBAP
          arktx TYPE arktx,    "VBAP
          kwmeng TYPE kwmeng,   "VBAP
          vrkme TYPE vrkme,            "VBAP
          UMVKZ type UMVKZ ,
          UMVKN type UMVKN,
          meins type meins,
          werks TYPE werks_ext,        "Centro para Buscar en Función ZMM_FM_CNSLTA_SERLOT
          lgort TYPE lgort_d,          "Almacen para Buscar en Función ZMM_FM_CNSLTA_SERLO
          ubic_fija TYPE ze_ubicacion, "vacio
          ubic_tmp TYPE	ze_ubicacion,  "vacio
          cant_contada TYPE	menge_d,   "vacio
          seriales TYPE	c LENGTH 1,    "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
          numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
          auart TYPE auart,            "EKKO Clase de documento de Ventas
    END OF ltp_vbap.
*}   REPLACE

  DATA :
*.....Tabla Interna para Busqueda de Pedidos de Venta Pendientes por Picking
           lti_vbup TYPE TABLE OF ltp_vbup,
*.....Tabla Interna de Posiciones de Pedidos de Venta
           lti_vbap TYPE TABLE OF ltp_vbap,
*.....Tabla Interna para Cabecera Pedidos de Venta
           lti_vbak TYPE TABLE OF ltp_vbak,
*.....Variable Estructura para Tabla de Mensajes
           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           lti_charg TYPE TABLE OF zstmm_0002,
           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
           lv_vbeln type vbeln,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_vbup> TYPE ltp_vbup,
                  <lfs_vbap> TYPE ltp_vbap,
                  <lfs_vbak> TYPE ltp_vbak,
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
      cod_modulo      = i_tipo_documento        "PROY
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido si Existe Información para el Usuario del Dispositivo
    IF detalles_usuario IS NOT INITIAL.
      move i_documento_origen to lv_vbeln.
*.....Función para Ajuste de Número
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = lv_vbeln
    importing
      output = lv_vbeln.

*.....Busqueda de Documentos de Pedidos de Ventas Pendientes por Picking
*{   REPLACE        ER3K900304                                        2
*\      SELECT vbeln posnr matnr arktx kwmeng  vrkme werks lgort
      SELECT vbeln posnr matnr arktx kwmeng vrkme UMVKZ UMVKN meins werks lgort
*}   REPLACE
           FROM  vbap
            INTO CORRESPONDING FIELDS OF TABLE lti_vbap
              WHERE werks EQ detalles_usuario-centro AND
                    lgort EQ detalles_usuario-almacen AND
                    vbeln EQ lv_vbeln.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0 .
*.....Ordeno la Tabla para el For All Entries
        SORT lti_vbap BY  vbeln posnr ASCENDING.
*.....Busco Pedidos de Venta Pendientes por Picking
        SELECT vbeln posnr lfgsa
            FROM vbup
            INTO TABLE lti_vbup
               FOR ALL ENTRIES IN lti_vbap
                 WHERE vbeln EQ lti_vbap-vbeln AND
                       posnr EQ lti_vbap-posnr ."AND
*                       gbsta NE 'C'.
*.....Verifico que Existan Registros
*        IF sy-subrc EQ 0.
*          LOOP AT lti_vbup ASSIGNING  <lfs_vbup>.
*            IF <lfs_vbup>-lfgsa NE 'C'.
*              CONTINUE.
*            ELSE.
*              DELETE lti_vbup INDEX sy-tabix.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.

*.....Verifico que Existan Registros a Procesar
      IF lti_vbup IS NOT INITIAL.
*Ordeno la Tabla
        SORT lti_vbap BY vbeln ASCENDING.
*.....Ordeno la Tabla
        SORT lti_vbup BY vbeln posnr ASCENDING.
*.....Ajusto Tabla de Posiciones con su Clase de Documento Respectiva
        SELECT vbeln auart
             FROM vbak
               INTO TABLE lti_vbak
                    FOR ALL ENTRIES IN lti_vbap
                            WHERE vbeln EQ lti_vbap-vbeln AND
                                  auart EQ i_clase_documento.

*.....Busco el Codigo de la Clase para El documento Recibido
        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
        IF sy-subrc EQ 0 .
*{   REPLACE        ER3K900304                                        3
*\          LOOP AT  lti_vbak ASSIGNING <lfs_vbak>  .
*\            DELETE lti_vbak WHERE auart NE <lfs_actividad>-cod_clase.
*\          ENDLOOP.
*          LOOP AT  lti_vbak ASSIGNING <lfs_vbak>  .
            DELETE lti_vbak WHERE auart NE <lfs_actividad>-cod_clase.
*          ENDLOOP.
*}   REPLACE
        ELSE.
          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.

*{   DELETE         ER3K900304                                        4
*\*.....Ajusto Información de la Tabla de Posiciones
*\        LOOP AT lti_vbap ASSIGNING <lfs_vbap>.
*\          READ TABLE lti_vbak WITH KEY vbeln = <lfs_vbap>-vbeln ASSIGNING <lfs_vbak>.
*\          IF sy-subrc EQ 0 .
*\            <lfs_vbap>-auart =  <lfs_vbak>-auart.
*\          ENDIF.
*\        ENDLOOP.
*}   DELETE

*.....Busco Registros que esten Pendiente por Picking con el Número Doc Recibido
        READ TABLE lti_vbup WITH KEY vbeln = lv_vbeln "i_documento_origen
                                    ASSIGNING <lfs_vbup>.
        IF sy-subrc EQ 0.
*-----------------------------------------------------------------------
*.....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*     Modificado por: Andrés Felipe Castro.
          LOOP AT lti_vbap ASSIGNING <lfs_vbap> WHERE  vbeln = <lfs_vbup>-vbeln.
            les_mara-matnr = <lfs_vbap>-matnr.
            APPEND les_mara TO lti_mara.
          ENDLOOP.

*{   REPLACE        ER3K900304                                        5
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
*{   INSERT         ER3K900304                                        6
  SORT lti_vbap by VBELN MATNR ASCENDING.
*}   INSERT
          LOOP AT lti_vbap ASSIGNING <lfs_vbap> WHERE  vbeln = <lfs_vbup>-vbeln.

            IF  sy-subrc EQ 0.
*.....Limpieza de Variables
              CLEAR: lti_return, lti_serno, lti_charg, lti_matnr.
*.....Inserto Registro con Número de Material
              l_e_matnr = <lfs_vbap>-matnr.
              APPEND l_e_matnr TO lti_matnr.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
              CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                EXPORTING
                  werk      = <lfs_vbap>-werks
                  lgort     = <lfs_vbap>-lgort
                  username  = i_usuario
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
                  <lfs_vbap>-seriales = 'X'.
                ELSE.
                  <lfs_vbap>-seriales = ' '.
                ENDIF.
*                ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                IF lti_charg  IS NOT INITIAL  .
                  APPEND LINES OF lti_charg TO c_lotes.
                  <lfs_vbap>-numero_lote = 'X'.
                ELSE.
                  <lfs_vbap>-numero_lote = ' '.
                ENDIF.
*                ENDIF.
*{   REPLACE        ER3K900304                                        7
*\*.....Limpieza de Variables
*\                CLEAR : lti_detalle, lti_mensajes.
*\*.....Función para Obtener EANs de un Material
*\                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                  EXPORTING
*\                    matnr      = <lfs_vbap>-matnr
*\                  TABLES
*\                    t_detalle  = lti_detalle
*\                    t_mensajes = lti_mensajes.
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores en los EAN´s del Material
*\                READ TABLE lti_mensajes  INDEX 1 ASSIGNING  <lfs_mensajes>.
*\                IF sy-subrc EQ 0 .
*\                  les_msg-num_doc = <lfs_mensajes>-num_doc.
*\                  les_msg-msg = <lfs_mensajes>-msg.
*\                  les_msg-type_msg = <lfs_mensajes>-msg.
*\                  APPEND les_msg TO c_mensajes.
*\                ELSE.
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\                  IF lti_detalle IS NOT INITIAL.
*\                    LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                      les_det_pos-num_entrega = <lfs_vbap>-vbeln.
*\                      les_det_pos-pos_entrega = <lfs_vbap>-posnr.
*\                      les_det_pos-material = <lfs_vbap>-matnr.
*\                      les_det_pos-desc_material = <lfs_vbap>-arktx.
*\                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
*\                      les_det_pos-ubic_fija = ' '.
*\                      les_det_pos-ubic_tmp =  ' '.
*\                      les_det_pos-cant_contada =  ' '.
*\*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*\*.....Realizar la conversión de la unidad de medida
*\                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                        EXPORTING
*\                          input          = <lfs_vbap>-vrkme
*\                          language       = 'S'
*\                        IMPORTING
*\                          output         = l_e_meinh
*\                        EXCEPTIONS
*\                          unit_not_found = 1
*\                          OTHERS         = 2.
*\                      IF l_e_meinh EQ '004'.
*\                        les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*\                      ELSE.
*\                        les_det_pos-uni_med_doc = l_e_meinh.
*\                      ENDIF.
*\                      les_det_pos-seriales = <lfs_vbap>-seriales.
*\                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*\*.....Datos de Detalle EAN
*\                      les_det_pos-ean = <lfs_detalle>-ean.
*\                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                      APPEND les_det_pos TO c_det_posiciones.
*\                      CLEAR les_det_pos.
*\                    ENDLOOP.
*\                  ELSE.
*\                    les_msg-num_doc = ' '.
*\                    CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_vbap>-matnr
*\                    INTO l_e_desc_error RESPECTING BLANKS.
*\                    les_msg-msg = l_e_desc_error.
*\                    les_msg-type_msg = 'E'.
*\                    APPEND les_msg TO c_mensajes.
*\                  ENDIF.
*\                ENDIF.
IF I_FLAG_AGRUP is not initial.
  CLEAR les_det_pos.
                    les_det_pos-num_entrega = <lfs_vbap>-vbeln.
*                      les_det_pos-pos_entrega = <lfs_vbap>-posnr.
                      les_det_pos-material = <lfs_vbap>-matnr.
                      les_det_pos-desc_material = <lfs_vbap>-arktx.

                      les_det_pos-ubic_fija = ' '.
                      les_det_pos-ubic_tmp =  ' '.
                      les_det_pos-cant_contada =  ' '.
*.... Convierto a unidad de medida base
                      IF <lfs_vbap>-VRKME NE <lfs_vbap>-MEINS.
                          <lfs_vbap>-kwmeng = ( <lfs_vbap>-kwmeng * <lfs_vbap>-UMVKZ ) / <lfs_vbap>-UMVKN.
                      ENDIF.
                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                        EXPORTING
                          input          = <lfs_vbap>-MEINS
                          language       = 'S'
                        IMPORTING
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_vbap>-MEINS.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
                      ENDIF.
                      les_det_pos-seriales = <lfs_vbap>-seriales.
                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
                      READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = <lfs_vbap>-matnr UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
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
                    matnr      = <lfs_vbap>-matnr
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
                      les_det_pos-num_entrega = <lfs_vbap>-vbeln.
                      les_det_pos-pos_entrega = <lfs_vbap>-posnr.
                      les_det_pos-material = <lfs_vbap>-matnr.
                      les_det_pos-desc_material = <lfs_vbap>-arktx.
                      les_det_pos-cant_por_pick = <lfs_vbap>-kwmeng.
                      les_det_pos-ubic_fija = ' '.
                      les_det_pos-ubic_tmp =  ' '.
                      les_det_pos-cant_contada =  ' '.
*                      les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
*.....Realizar la conversión de la unidad de medida
                      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                        EXPORTING
                          input          = <lfs_vbap>-vrkme
                          language       = 'S'
                        IMPORTING
                          output         = l_e_meinh
                        EXCEPTIONS
                          unit_not_found = 1
                          OTHERS         = 2.
                      IF l_e_meinh EQ '004'.
                        les_det_pos-uni_med_doc = <lfs_vbap>-vrkme.
                      ELSE.
                        les_det_pos-uni_med_doc = l_e_meinh.
                      ENDIF.
                      les_det_pos-seriales = <lfs_vbap>-seriales.
                      les_det_pos-numero_lote = <lfs_vbap>-numero_lote.
*.....Datos de Detalle EAN
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      APPEND les_det_pos TO c_det_posiciones.
                      CLEAR les_det_pos.
                    ENDLOOP.
                  ELSE.
                    les_msg-num_doc = ' '.
                    CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_vbap>-matnr
                    INTO l_e_desc_error RESPECTING BLANKS.
                    les_msg-msg = l_e_desc_error.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                  ENDIF.
                ENDIF.
                ENDIF.
*}   REPLACE
              ELSE.
                les_msg-num_doc = <lfs_vbap>-vbeln.
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
* Descripción  : Obtener la Ubicación sugerida del Documentos
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
refresh c_mensajes.
*... Consulto el detalle del documento para traer los materiales
call method me->zif_lgtica_documento~get_detalle
  exporting
    i_documento_origen = i_documento_origen
    i_tipo_documento   = i_tipo_documento
    i_clase_documento  = i_clase_documento
    i_usuario          = i_usuario
  changing
    c_det_posiciones   = c_det_posiciones
    c_seriales         = c_seriales
    c_lotes            = c_lotes
    c_mensajes         = c_mensajes
    .
*... ordeno por numero de documento y material
sort c_det_posiciones by num_entrega material.
*... Borro los materilales duplicados.
delete adjacent duplicates from c_det_posiciones comparing num_entrega material.

*... Valido que hayan datos
  if not c_det_posiciones[] is initial.
*... Consulto las ubicaciones para todos los materiales
        select *
        into table ti_zmm_t_ubicacion
        from zmm_t_ubicacion
        for all entries in c_det_posiciones
        where material eq c_det_posiciones-material
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
les_ubicacion_suge-cantidad  = les_ubicacion-cantidad.
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

endif.
endmethod.


METHOD ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT.
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
*.....Metodo para Setiar Atributos de ventas
  CALL METHOD me->zif_lgtica_documento~set_ventas
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
* Ventas
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de compras
*{   REPLACE        ER3K900279                                        1
*\  DATA: les_ventas TYPE vbak.
  DATA: les_ventas TYPE ZESD_CAB_PED.
*}   REPLACE
*.... Estructura para el manejo del picking
  DATA: les_header_picking TYPE zedsd_picking.

*.... Estructuras para el manejo de las entregas
  DATA: lti_entregas TYPE STANDARD TABLE OF zesd_det_ent_pkg,
        les_entregas TYPE zesd_det_ent_pkg.

*.... Copio el atributo de las entregas a la estructura
  MOVE zif_lgtica_documento~t_entregas TO lti_entregas.


  READ TABLE cabeceras_ventas INTO les_ventas INDEX 1.

  IF sy-subrc EQ 0.
    les_header_picking-mandt = sy-mandt.
*les_header_picking-PICKNUM =
*les_header_picking-PROCESO =
*les_header_picking-SUBPROCESO =
les_header_picking-VBELN = les_ventas-vbeln.
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
* Ventas
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de ventas detalle
  data: les_ventas_dt type ZESD_POSICIONES_VENTAS.

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
          clear:les_pos_picking,les_ventas_dt.
*.... Leo la detalle de ventas
          read table DETALLES_VENTAS into les_ventas_dt with key matnr = les_lips-matnr posnr = les_lips-KDPOS .


          les_pos_picking-MATERIAL = les_lips-matnr.
"consulto posición superior si esta particionado por lotes.
          READ TABLE lti_lips into les_lips_aux with key matnr = les_lips-matnr VBELN = les_entregas-entrega
          POSNR = les_lips-UECHA.
          IF sy-subrc eq 0 .
            les_pos_picking-POSICION = les_lips_aux-posnr.
          else.
            les_pos_picking-POSICION = les_lips-POSNR.
          ENDIF.
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
