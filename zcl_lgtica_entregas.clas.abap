class ZCL_LGTICA_ENTREGAS definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_ENTREGAS
*"* do not include other source files here!!!
public section.

  methods ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GENERAR_PACKING
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA_PACKING
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_UBICACION_SUGERIDA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~SET_ENTREGAS
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
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE_PACKING
    redefinition .
protected section.
*"* protected components of class ZCL_LGTICA_ENTREGAS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_PEDIDOS_VENTA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_ENTREGAS IMPLEMENTATION.


METHOD ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Entregas para Proceso Pedidos de Ventas
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 04.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 04.07.2014    ER6K906899    Marco Suarez        Creación
*-------------------------------------------------------------------------------*


  DATA:
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
*.....Variable para Mensaje de Respusta
        l_e_msg TYPE string,
*.....Variable Fecha
        l_e_fecha TYPE sy-datum,

        lo_pick TYPE REF TO zif_lgtica_pickable,
        lo_pick_clas TYPE REF TO zcl_lgtica_picking.

*.....Field Simbols para Procesamiento de Información de Compras
  FIELD-SYMBOLS: <lfs_vbap> TYPE zesd_posiciones_ventas,
*{   REPLACE        ER3K900309                                        1
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
*.....Verifico el Número de Entregas a Generar
    IF detalles_ventas[] IS NOT INITIAL .
      LOOP AT detalles_ventas  ASSIGNING <lfs_vbap>.
*.....Para el Centro Generar la Entrega
        AT END OF werks.

          l_e_fecha = sy-datum.

          CLEAR :les_key_enque_read, lti_key_enque_read, les_entregas,
                 lti_vbls.
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
                e_vbsk            = lti_vbsk.
            IF lti_vbls[] IS NOT INITIAL .
*.....Ordeno Tabla
              SORT  lti_vbls BY  vbeln_lif posnr_lif ASCENDING.
*.....Elimino Registros Duplicados
*            DELETE ADJACENT DUPLICATES FROM lti_vbls COMPARING vbeln_lif posnr_lif.
*.....Actualizo Base de Datos
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.

*.....Up Cast
*            lo_pick = me.
*
*            CALL METHOD zcl_lgtica_picking=>create_by_pkble_ref
*              EXPORTING
*                i_ref_pickable = lo_pick.
              LOOP AT lti_vbls ASSIGNING <lfs_vbls> .
                les_entregas-documento =  <lfs_vbak>-vbeln.
                les_entregas-entrega =  <lfs_vbls>-vbeln_lif.
                APPEND les_entregas TO c_entregas.
                CLEAR : les_entregas.
              ENDLOOP.
            ELSE.
              CONCATENATE 'Error en Generación de Entrega, Para el Doc.' space <lfs_vbak>-vbeln space
                        ' Centro Nro' space <lfs_vbap>-werks space  'Verifique Información'
                         INTO l_e_msg RESPECTING BLANKS.
              les_msg-num_doc = <lfs_vbak>-vbeln.
              les_msg-msg = l_e_msg.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
            ENDIF.
          ENDIF.
        ENDAT.
      ENDLOOP.
*.....Elimino Registros Duplicados
      DELETE ADJACENT DUPLICATES FROM c_entregas COMPARING documento entrega.
*    me->entregas = c_entregas.
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


METHOD zif_lgtica_documento~get_cabecera.
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
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunnr TYPE likp-kunnr, "Destinatario
         END OF ltp_likp.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.

  DATA :
*.....Variable Estructura con Información del Documento
        les_cab_ref TYPE zesd_cab_ref,
*.....Tabla Interna para Datos de entrega
        lti_likp TYPE TABLE OF ltp_likp,
        les_likp TYPE ltp_likp,
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking TYPE TABLE OF zmm_t_picking,
        les_picking TYPE zmm_t_picking,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing TYPE TABLE OF zmm_t_packing,
        les_packing TYPE zmm_t_packing,


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
           lv_vbeln TYPE vbeln,
           lv_kunnr TYPE kunnr,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_picking> TYPE zmm_t_picking,
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_likp> TYPE ltp_likp,
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

      IF i_documento_origen IS NOT INITIAL and i_cod_cliente is initial.
        MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_vbeln
          IMPORTING
            output = lv_vbeln.

*.....Busqueda de entregas para El Usuario Según el Centro, Almacen y Documento Origne que tengan Asignados
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                    FROM  likp
                    INTO TABLE lti_likp
                    WHERE vbeln EQ lv_vbeln
                    and vstel EQ detalles_usuario-centro.

      ELSEIF i_cod_cliente IS NOT INITIAL.
        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                           FROM  likp
                           INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
                           WHERE kunnr EQ lv_kunnr
                           and vstel EQ detalles_usuario-centro.
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
       SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                          FROM  likp
                          INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
                          WHERE erdat IN rl_erdat
                          and vstel EQ detalles_usuario-centro.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_likp[] IS NOT  INITIAL.
*.....verificar documentos despachados para entregas
        SELECT *
           FROM zmm_t_packing
             INTO TABLE lti_packing
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
                        estado EQ 'CERRADO'.

*.....Busqueda de documentos Pendientes por Packing para entregas
        SELECT *
           FROM zmm_t_picking
             INTO TABLE lti_picking
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
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
*.....Consulto Informacion del Cliente
            SELECT kunnr name1
                  FROM  kna1
                   INTO TABLE lti_kna1
                      FOR ALL ENTRIES IN lti_likp
                          WHERE kunnr EQ lti_likp-kunnr.

*.....Recorro Tabla de Cabeceras
            LOOP AT lti_picking ASSIGNING <lfs_picking>.
              READ TABLE lti_packing WITH KEY vbeln2 = <lfs_picking>-vbeln2 into les_packing.
              IF sy-subrc eq 0.
                CONTINUE.
              ENDIF.
              READ TABLE lti_likp WITH KEY vbeln = <lfs_picking>-vbeln2
                                  ASSIGNING <lfs_likp>.
              IF <lfs_likp> IS ASSIGNED.
                READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                  ASSIGNING <lfs_kna1>.
                IF <lfs_kna1> IS ASSIGNED.
                  IF <lfs_picking>-vbeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-vbeln.
                  ENDIF.

                  IF <lfs_picking>-ebeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-ebeln.
                  ENDIF.
                  les_cab_ref-fecha_crea  = <lfs_picking>-fecha.
*            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
                  les_cab_ref-cod_cliente = <lfs_likp>-kunnr.
                  les_cab_ref-nom_cliente = <lfs_kna1>-name1.
                  les_cab_ref-vbeln = <lfs_picking>-vbeln2.
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
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunnr TYPE likp-kunnr, "Destinatario
         END OF ltp_likp.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.

  DATA :
*.....Variable Estructura con Información del Documento
        les_cab_ref TYPE zesd_cab_ref,
*.....Tabla Interna para Datos de entrega
        lti_likp TYPE TABLE OF ltp_likp,
        les_likp TYPE ltp_likp,
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking TYPE TABLE OF zmm_t_picking,
        les_picking TYPE zmm_t_picking,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing TYPE TABLE OF zmm_t_packing,
        les_packing TYPE zmm_t_packing,


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
           lv_vbeln TYPE vbeln,
           lv_kunnr TYPE kunnr,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_picking> TYPE zmm_t_picking,
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_likp> TYPE ltp_likp,
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

      IF i_documento_origen IS NOT INITIAL and i_cod_cliente is initial.
        MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_vbeln
          IMPORTING
            output = lv_vbeln.

*.....Busqueda de entregas para El Usuario Según el Centro, Almacen y Documento Origne que tengan Asignados
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                    FROM  likp
                    INTO TABLE lti_likp
                    WHERE vbeln EQ lv_vbeln
                    and vstel EQ detalles_usuario-centro.
      ELSEIF i_cod_cliente IS NOT INITIAL.
        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.
*{   INSERT         ER3K900332                                        3
*        l_e_fecha_fin = sy-datum.
*        l_e_fecha_inicio  = l_e_fecha_fin - 30.
**.....Creo Rango de Fechas
*        ls_erdat-sign = 'I'.
*        ls_erdat-option = 'BT'.
*        ls_erdat-low = l_e_fecha_inicio.
*        ls_erdat-high = l_e_fecha_fin.
*        APPEND ls_erdat TO rl_erdat.
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

*}   INSERT
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                           FROM  likp
                           INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
*{   REPLACE        ER3K900332                                        2
*\                           WHERE kunnr EQ lv_kunnr
                           WHERE kunnr EQ lv_kunnr
*          AND
**                           WHERE  kunag EQ lv_kunnr AND
*                             erdat IN rl_erdat
*}   REPLACE
                           and vstel EQ detalles_usuario-centro.
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
       SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                          FROM  likp
                          INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
                          WHERE erdat IN rl_erdat
                          and vstel EQ detalles_usuario-centro.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_likp[] IS NOT  INITIAL.
*{   REPLACE        ER3K900332                                        1
*\*.....verificar documentos despachados para entregas
*\        SELECT *
*\           FROM zmm_t_packing
*\             INTO TABLE lti_packing
*\               FOR ALL ENTRIES IN lti_likp
*\                  WHERE vbeln2 EQ lti_likp-vbeln AND
*\                        estado EQ 'CERRADO'.
*\
*\*.....Busqueda de documentos Pendientes por Packing para entregas
*\        SELECT *
*\           FROM zmm_t_picking
*\             INTO TABLE lti_picking
*\               FOR ALL ENTRIES IN lti_likp
*\                  WHERE vbeln2 EQ lti_likp-vbeln AND
*\                        estado EQ 'CERRADO'.
*.....verificar documentos despachados para entregas
        SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR  RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD
               USRMOD UNAMOD ESTADO
           FROM zmm_t_packing
             INTO TABLE lti_packing
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
                        estado EQ 'CERRADO'.

*.....Busqueda de documentos Pendientes por Packing para entregas
        SELECT MANDT PICKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD
               USRMOD UNAMOD ESTADO
           FROM zmm_t_picking
             INTO TABLE lti_picking
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
                        estado EQ 'CERRADO'.
*}   REPLACE


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
*.....Consulto Informacion del Cliente
            SELECT kunnr name1
                  FROM  kna1
                   INTO TABLE lti_kna1
                      FOR ALL ENTRIES IN lti_likp
                          WHERE kunnr EQ lti_likp-kunnr.

*.....Recorro Tabla de Cabeceras
            LOOP AT lti_picking ASSIGNING <lfs_picking>.
              READ TABLE lti_packing WITH KEY vbeln2 = <lfs_picking>-vbeln2 into les_packing.
              IF sy-subrc eq 0.
                CONTINUE.
              ENDIF.
              READ TABLE lti_likp WITH KEY vbeln = <lfs_picking>-vbeln2
                                  ASSIGNING <lfs_likp>.
              IF <lfs_likp> IS ASSIGNED.
                READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                  ASSIGNING <lfs_kna1>.
                IF <lfs_kna1> IS ASSIGNED.
                  IF <lfs_picking>-vbeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-vbeln.
                  ENDIF.

                  IF <lfs_picking>-ebeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-ebeln.
                  ENDIF.
                  les_cab_ref-fecha_crea  = <lfs_picking>-fecha.
*            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
                  les_cab_ref-cod_cliente = <lfs_likp>-kunnr.
                  les_cab_ref-nom_cliente = <lfs_kna1>-name1.
                  les_cab_ref-vbeln = <lfs_picking>-vbeln2.
                  APPEND les_cab_ref TO c_det_cabecera.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ELSE.
*          Parametrización de la actividad
*{   REPLACE        ER3K900332                                        4
*\            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
            MESSAGE S000(ZCLPACK) into les_msg-msg .
*}   REPLACE
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.
        ENDIF.
      ELSE.
*{   REPLACE        ER3K900332                                        5
*\        les_msg-msg = 'No Existen Documentos Pendientes por Packing'.
        MESSAGE S001(ZCLPACK) into les_msg-msg .
*}   REPLACE
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
    ELSE.
*{   REPLACE        ER3K900332                                        6
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
* Descripción  : Detalle de Documentos Entregas
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunag TYPE likp-kunag, "Destinatario
         END OF ltp_likp.

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

  DATA :
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
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
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
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

           lv_vbeln TYPE vbeln,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

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
      MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_vbeln
        IMPORTING
          output = lv_vbeln.



*.... Se toman las posiciones de la entrega ingresada
      SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
      INTO TABLE lti_lips
        FROM lips
        WHERE vbeln EQ lv_vbeln .
*        AND
*        werks EQ detalles_usuario-centro AND
*        lgort EQ detalles_usuario-almacen.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0 .
*.....Ordeno la Tabla
        SORT lti_lips BY  vbeln posnr ASCENDING.
*.... Obtengo el picknum de la entrega
        CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
          EXPORTING
            i_entrega = lv_vbeln
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
*.....Inicia Proceso de Actualización de Entrega
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
*.... Filtro seriales y lotes de la entrega
          DELETE lti_picking_lot WHERE vbeln2 NE lv_vbeln.
          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.

          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lfs_picking_det>-material
        IMPORTING
          output = <lfs_picking_det>-material.

*....    Valido material pertenezca a la entrega ( Centro y Almacen )
            READ TABLE lti_lips INTO les_lips WITH KEY  matnr = <lfs_picking_det>-material.
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
                    les_det_pos-num_entrega = les_picking_cab-vbeln2.
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


method ZIF_LGTICA_DOCUMENTO~GET_DETALLE_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos Entregas
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunag TYPE likp-kunag, "Destinatario
         END OF ltp_likp.

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

  DATA :
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
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
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
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

           lv_vbeln TYPE vbeln,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.
*{   INSERT         ER3K900332                                        2
     DATA: lti_mara TYPE TABLE OF mara ,
           les_mara TYPE mara.

*}   INSERT

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
      MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_vbeln
        IMPORTING
          output = lv_vbeln.



*.... Se toman las posiciones de la entrega ingresada
      SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
      INTO TABLE lti_lips
        FROM lips
        WHERE vbeln EQ lv_vbeln .
*        AND
*        werks EQ detalles_usuario-centro AND
*        lgort EQ detalles_usuario-almacen.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0 .
*.....Ordeno la Tabla
        SORT lti_lips BY  vbeln posnr ASCENDING.
*.... Obtengo el picknum de la entrega
        CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
          EXPORTING
            i_entrega = lv_vbeln
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
*.....Inicia Proceso de Actualización de Entrega
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
*.... Filtro seriales y lotes de la entrega
*{   DELETE         ER3K900332                                        4
*\          DELETE lti_picking_lot WHERE vbeln2 NE lv_vbeln.
*\          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.
*}   DELETE
*{   INSERT         ER3K900332                                        1
          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.
          IF lti_picking_lot is INITIAL.
            Select MANDT DOCUMENTO TIPO_DOC  MATNR LOTE UBICACION_FI CANTIDAD UMC VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM CANTIDAD_USO UMC_USO flag_lot
              from ZMM_T_PCK_LOT
              into table lti_picking_lot
              where DOCUMENTO eq les_picking_cab-vbeln.
          ENDIF.
          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
            les_mara-matnr = <lfs_picking_det>-material.
            APPEND les_mara TO lti_mara.
          ENDLOOP.
          IF NOT lti_mara is INITIAL.
*  .....Función para Obtener EANs de un Material
          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = c_eans
              t_mara = lti_mara.
          ENDIF.

*}   INSERT

          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lfs_picking_det>-material
        IMPORTING
          output = <lfs_picking_det>-material.

*....    Valido material pertenezca a la entrega ( Centro y Almacen )
            READ TABLE lti_lips INTO les_lips WITH KEY  matnr = <lfs_picking_det>-material.
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
*{   REPLACE        ER3K900332                                        3
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
*\                    les_det_pos-num_entrega = les_picking_cab-vbeln2.
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
*\                        les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
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
                        les_det_pos-num_entrega = les_picking_cab-vbeln2.
*                        les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
                        les_det_pos-material = <lfs_picking_det>-material.

                        les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
                        les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
                        les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
                        les_det_pos-cant_contada =  ' '.
                        les_det_pos-uni_med_doc = <lfs_picking_det>-umc.
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
                        les_det_pos-num_entrega = les_picking_cab-vbeln2.
                        les_det_pos-pos_entrega = <lfs_picking_det>-posicion.
                        les_det_pos-material = <lfs_picking_det>-material.
                        les_det_pos-desc_material = <lfs_detalle>-maktx.
                        les_det_pos-cant_por_pick = <lfs_picking_det>-cantidad.
                        les_det_pos-ubic_fija = <lfs_picking_det>-ubicacion_fi.
                        les_det_pos-ubic_tmp =  <lfs_picking_det>-ubicacion_tmp.
                        les_det_pos-cant_contada =  ' '.
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


endmethod.


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
DATA: les_msg type zesd_msg_picking.
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
IF detalles_usuario is initial.
c_mensajes = mensajes.
else.
REFRESH c_mensajes.
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

ENDIF.

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
*.... Documento de origen para set de ventas y/o compras
      l_e_doc_org TYPE ze_documento,
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
*.... Estructura para llenar la información de retorno de la entrega
          les_entrega TYPE zesd_det_ent_pkg,
*.... Variable para almacenar el número de documento
          v_VBELN TYPE VBELN,
*.....Estructura para Cabecera Ventas
          les_cab_ventas TYPE vbak,
          les_cab_compras TYPE ekko,

*.....Rango para Números de Documento
          rl_numero TYPE RANGE OF VBELN,
          ls_numero LIKE LINE OF rl_numero.

*.....Ajusto Numero de Documento
  IF i_documento_origen IS NOT INITIAL .
    MOVE i_documento_origen TO v_VBELN.
*.....Función para Ajuste de Número
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_VBELN
      IMPORTING
        output = v_VBELN.
*.....Creo Rango de Documento
    ls_numero-sign = 'I'.
    ls_numero-option = 'EQ'.
    ls_numero-low = v_VBELN.
    APPEND ls_numero TO rl_numero.
  ENDIF.

*.... Se toman las entregas asociadas al Número de documento ingresado
  SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
  INTO TABLE lti_lips
    FROM lips
    WHERE VBELN IN rl_numero.

  IF sy-subrc EQ 0.

*.... Recorro las entregas encontradas y las ingreso en el atributo de T_ENTREGAS
    LOOP AT  lti_lips INTO les_lips.
      CLEAR : les_entrega.
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
*        les_entrega-customer = les_cab_ventas-kunnr.
*        les_entrega-orderid = les_cab_ventas-aufnr.
      les_entrega-order_itno = les_lips-posnr.
      APPEND les_entrega TO zif_lgtica_documento~t_entregas.

    ENDLOOP.
*.....Ordeno Tabla
    SORT zif_lgtica_documento~t_entregas BY documento entrega ASCENDING.

    MOVE les_entrega-vgbel TO l_e_doc_org.
    IF l_e_doc_org IS NOT INITIAL.
*.....Metodo para Setiar Atributos de ventas
      CALL METHOD me->zif_lgtica_documento~set_ventas
        EXPORTING
*          i_clase_documento  = i_clase_documento
          i_documento_origen = l_e_doc_org.

*.....Metodo para Setiar Atributos de compras
      CALL METHOD me->zif_lgtica_documento~set_compras
        EXPORTING
*          i_clase_documento  = i_clase_documento
          i_documento_origen = l_e_doc_org.
    ENDIF.

  ENDIF.

ENDMETHOD.


method ZIF_LGTICA_PACKING~GET_CABECERA_PACKING.
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
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunnr TYPE likp-kunnr, "Destinatario
         END OF ltp_likp.

*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.

  DATA :
*.....Variable Estructura con Información del Documento
        les_cab_ref TYPE zesd_cab_ref,
*.....Tabla Interna para Datos de entrega
        lti_likp TYPE TABLE OF ltp_likp,
        les_likp TYPE ltp_likp,
*.....Tabla Interna para Datos de cabecera de picking
        lti_picking TYPE TABLE OF zmm_t_picking,
        les_picking TYPE zmm_t_picking,
*.....Tabla Interna para Datos de cabecera de packing
        lti_packing TYPE TABLE OF zmm_t_packing,
        les_packing TYPE zmm_t_packing,


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
           lv_vbeln TYPE vbeln,
           lv_kunnr TYPE kunnr,
*.....Fecha Final
           l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS: <lfs_picking> TYPE zmm_t_picking,
                 <lfs_kna1> TYPE ltp_kna1,
                 <lfs_likp> TYPE ltp_likp,
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

      IF i_documento_origen IS NOT INITIAL and i_cod_cliente is initial.
        MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_vbeln
          IMPORTING
            output = lv_vbeln.

*.....Busqueda de entregas para El Usuario Según el Centro, Almacen y Documento Origne que tengan Asignados
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                    FROM  likp
                    INTO TABLE lti_likp
                    WHERE vbeln EQ lv_vbeln
                    and vstel EQ detalles_usuario-centro.
      ELSEIF i_cod_cliente IS NOT INITIAL.
        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.
        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                           FROM  likp
                           INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
                           WHERE kunnr EQ lv_kunnr
                           and vstel EQ detalles_usuario-centro.
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
       SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                          FROM  likp
                          INTO TABLE lti_likp
*                      WHERE werks EQ detalles_usuario-centro AND
*                            lgort EQ detalles_usuario-almacen AND
                          WHERE erdat IN rl_erdat
                          and vstel EQ detalles_usuario-centro.
      ENDIF.

*.....Verifico que Existan Registros
      IF lti_likp[] IS NOT  INITIAL.
*.....verificar documentos despachados para entregas
        SELECT *
           FROM zmm_t_packing
             INTO TABLE lti_packing
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
                        estado EQ 'CERRADO'.

*.....Busqueda de documentos Pendientes por Packing para entregas
        SELECT *
           FROM zmm_t_picking
             INTO TABLE lti_picking
               FOR ALL ENTRIES IN lti_likp
                  WHERE vbeln2 EQ lti_likp-vbeln AND
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
*.....Consulto Informacion del Cliente
            SELECT kunnr name1
                  FROM  kna1
                   INTO TABLE lti_kna1
                      FOR ALL ENTRIES IN lti_likp
                          WHERE kunnr EQ lti_likp-kunnr.

*.....Recorro Tabla de Cabeceras
            LOOP AT lti_picking ASSIGNING <lfs_picking>.
              READ TABLE lti_packing WITH KEY vbeln2 = <lfs_picking>-vbeln2 into les_packing.
              IF sy-subrc eq 0.
                CONTINUE.
              ENDIF.
              READ TABLE lti_likp WITH KEY vbeln = <lfs_picking>-vbeln2
                                  ASSIGNING <lfs_likp>.
              IF <lfs_likp> IS ASSIGNED.
                READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                  ASSIGNING <lfs_kna1>.
                IF <lfs_kna1> IS ASSIGNED.
                  IF <lfs_picking>-vbeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-vbeln.
                  ENDIF.

                  IF <lfs_picking>-ebeln IS NOT INITIAL.
                    les_cab_ref-num_doc = <lfs_picking>-ebeln.
                  ENDIF.
                  les_cab_ref-fecha_crea  = <lfs_picking>-fecha.
*            les_cab_ref-tipo_doc    = <lfs_vbak>-auart.
                  les_cab_ref-cod_cliente = <lfs_likp>-kunnr.
                  les_cab_ref-nom_cliente = <lfs_kna1>-name1.
                  les_cab_ref-vbeln = <lfs_picking>-vbeln2.
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

endmethod.


method ZIF_LGTICA_PACKING~GET_DETALLE_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos Entregas
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Typo para Tabla LIKP
  TYPES : BEGIN OF ltp_likp,
          vbeln TYPE likp-vbeln,  "Entrega
          ernam TYPE likp-ernam,  "Creado por
          erzet TYPE likp-erzet,  "Hora
          erdat TYPE likp-erdat,  "Creado el
          bzirk TYPE likp-bzirk,  "Zona de ventas
          vstel TYPE likp-vstel,  "Pto.expedic./Pto.recepción
          vkorg TYPE likp-vkorg,  "Organización ventas
          lfart TYPE likp-lfart,  "Clase de entrega
          autlf TYPE likp-autlf,
          lifnr TYPE likp-lifnr,
          kunag TYPE likp-kunag, "Destinatario
         END OF ltp_likp.

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

  DATA :
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
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
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
*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

           lv_vbeln TYPE vbeln,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string.

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
      MOVE i_documento_origen TO lv_vbeln.
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_vbeln
        IMPORTING
          output = lv_vbeln.



*.... Se toman las posiciones de la entrega ingresada
      SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
      INTO TABLE lti_lips
        FROM lips
        WHERE vbeln EQ lv_vbeln .
*        AND
*        werks EQ detalles_usuario-centro AND
*        lgort EQ detalles_usuario-almacen.
*.....Verifico que Existan Registros
      IF sy-subrc EQ 0 .
*.....Ordeno la Tabla
        SORT lti_lips BY  vbeln posnr ASCENDING.
*.... Obtengo el picknum de la entrega
        CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
          EXPORTING
            i_entrega = lv_vbeln
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
*.....Inicia Proceso de Actualización de Entrega
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
*.... Filtro seriales y lotes de la entrega
          DELETE lti_picking_lot WHERE vbeln2 NE lv_vbeln.
          DELETE lti_picking_ser WHERE vbeln2 NE lv_vbeln.

          LOOP AT lti_picking_det ASSIGNING <lfs_picking_det> .
*.....Función para Ajuste de Número
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lfs_picking_det>-material
        IMPORTING
          output = <lfs_picking_det>-material.

*....    Valido material pertenezca a la entrega ( Centro y Almacen )
            READ TABLE lti_lips INTO les_lips WITH KEY  matnr = <lfs_picking_det>-material.
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
                    les_det_pos-num_entrega = les_picking_cab-vbeln2.
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

endmethod.


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
*.....Variable de Ref a la Clase Picknum
        lo_lgtica_picking TYPE REF TO zcl_lgtica_picking,
*.....Información de Cabecera del Documento Picking
        les_picking_cab TYPE zedsd_picking,
*.....Número de Pickmun
        l_e_piknum TYPE zed_picknum.


*.... Copio el atributo de las entregas a la estructura
  MOVE zif_lgtica_documento~t_entregas TO lti_entregas.
*.... Ordeno la tabla y eliminimo repetidos
  sorT lti_entregas BY entrega.

  DELETE ADJACENT DUPLICATES FROM lti_entregas COMPARING ENTREGA.

  READ TABLE lti_entregas INTO les_entregas INDEX 1.
*.....Consulto el Pikum de la Entrega
  CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
    EXPORTING
      i_entrega = les_entregas-entrega
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

*.... Copio el atributo de las entregas a la estructura
  MOVE zif_lgtica_documento~t_entregas TO lti_entregas.
*.... Ordeno la tabla y eliminimo repetidos
  sorT lti_entregas BY entrega.

  DELETE ADJACENT DUPLICATES FROM lti_entregas COMPARING ENTREGA.

  READ TABLE lti_entregas INTO les_entregas INDEX 1.
*.....Consulto el Pikum de la Entrega
  CALL METHOD zcl_lgtica_picking=>get_data_by_entrega
    EXPORTING
      i_entrega = les_entregas-entrega
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
* Ventas
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de compras
*{   REPLACE        ER3K900309                                        1
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

  sorT lti_entregas BY entrega.

  DELETE ADJACENT DUPLICATES FROM lti_entregas COMPARING ENTREGA.

*.... Leo la cabecera de ventas
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

      LOOP AT lti_lips into les_lips where VBELN = les_entregas-entrega and LFIMG > 0.
          clear:les_pos_picking,les_ventas_dt.
*.... Leo la detalle de ventas
          read table DETALLES_VENTAS into les_ventas_dt with key matnr = les_lips-matnr posnr = les_lips-KDPOS .


          "consulto posición superior si esta particionado por lotes.
          READ TABLE lti_lips into les_lips_aux with key matnr = les_lips-matnr VBELN = les_entregas-entrega
          POSNR = les_lips-UECHA.
          IF sy-subrc eq 0 .
            les_pos_picking-POSICION = les_lips_aux-posnr.
          else.
            les_pos_picking-POSICION = les_lips-POSNR.
          ENDIF.

          les_pos_picking-MATERIAL = les_lips-matnr.
          les_pos_picking-CANTIDAD = les_lips-LFIMG.
          les_pos_picking-UMC = les_lips-VRKME. " unidad de medida venta
*          les_pos_picking-UMC = les_lips-MEINS. " unidad de medida base
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
