class ZCL_LGTICA_DESPACHO definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_DESPACHO
*"* do not include other source files here!!!
public section.

  data DESPNUM type ZED_DESPNUM .
  data DETALLES_USUARIO type ZMM_T_IMP_ETIQ .
  data TIPO_DOCUMENTO type ZE_TIPO_DOC .
  data MENSAJES type ZTTSD_MSG_DESPACHO .

  methods CONSTRUCTOR
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_USUARIO type ZED_USUARIO_MOVIL .
  methods GET_DETALLES_USUARIO
    importing
      !I_USUARIO type ZED_USUARIO_MOVIL
    exporting
      !E_DET_USUARIO type ZMM_T_IMP_ETIQ
    changing
      !C_MENSAJES type ZTTSD_MSG_DESPACHO .
  methods GET_CABECERA
    importing
      !I_FEC_INI_BUS type ZE_FECHAS
      !I_FEC_FIN_BUS type ZE_FECHAS
      !I_TIPO_DOCUMENTO type ZE_TIPO_DOC
      !I_CLASE_DOCUMENTO type ZE_CLAS_DOCUMENTO
      !I_DOCUMENTO_ORIGEN type ZE_DOCUMENTO
      !I_USUARIO type ZED_USUARIO_MOVIL
      !I_COD_CLIENTE type ZE_KUNNR_SUP
    changing
      !C_DET_CABECERA type ZTTSD_CAB_DPC
      !C_MENSAJES type ZTTSD_MSG_DESPACHO .
  methods GET_DETALLE
    importing
      !I_DOCUMENTO_ORIGEN type ZE_DOCUMENTO
      !I_TIPO_DOCUMENTO type ZE_TIPO_DOC
      !I_CLASE_DOCUMENTO type ZE_CLAS_DOCUMENTO
      !I_USUARIO type ZED_USUARIO_MOVIL
    changing
      !C_DET_POSICIONES type ZTTSD_DET_DPC
      !C_MENSAJES type ZTTSD_MSG_DESPACHO .
  class-methods GUARDAR_DESPACHO
    importing
      !I_USUARIO type ZED_USUARIO_MOVIL
    changing
      !C_DETAIL type ZTTSD_DET_DPC
      !C_MENSAJES type ZTTSD_MSG_DESPACHO .
protected section.
*"* protected components of class ZCL_LGTICA_DESPACHO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_DESPACHO
*"* do not include other source files here!!!

  data S_DESPACHO type ZTTSD_CAB_DPC .
ENDCLASS.



CLASS ZCL_LGTICA_DESPACHO IMPLEMENTATION.


method CONSTRUCTOR.
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
  tipo_documento = i_tipo_doc.

*.....Llamado a Metodo para Obtener los Detalles de Usuario
  CALL METHOD me->get_detalles_usuario
    EXPORTING
      i_usuario     = i_usuario
    IMPORTING
      e_det_usuario = detalles_usuario
    CHANGING
      c_mensajes    = mensajes.
endmethod.


METHOD get_cabecera.
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
*{   INSERT         ER3K900346                                        4
*.....Typo para Datos de Reservas de Ordenes
  TYPES: BEGIN OF ltp_rkpf,
         rsnum TYPE EBELN,    "Número de la reserva/las necesidades secundarias
         wempf TYPE wempf,    "Destinatario de mercancías
         kunnr TYPE ekunn,    "Número de cuenta del cliente
         aufnr TYPE aufnr,    "Número de orden
        END OF ltp_rkpf.
*}   INSERT
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
*.....Typo para Datos de estado conteo
  TYPES : BEGIN OF ltp_est_conteo,
          numero_doc TYPE ze_documento,     "Nº de cliente 1
*          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_est_conteo.
*.....Typo para Datos de Cliente KNA1
  TYPES : BEGIN OF ltp_kna1,
          kunnr TYPE kunnr,     "Nº de cliente 1
          name1 TYPE name1_gp,                              "Nombre 1
         END OF ltp_kna1.
  DATA :
*{   INSERT         ER3K900346                                        5
*.....Tabla Interna con datos Basico de Reservas
        lti_rkpf TYPE TABLE OF ltp_rkpf,
        les_rkpf TYPE ltp_rkpf,
*}   INSERT
*... tabla interna para la cabecera del documento de packing
   lti_packing TYPE STANDARD TABLE OF zmm_t_packing,
*... Estructura para la cabecera del documento de packing
   les_packing TYPE zmm_t_packing,
*... Tabla interna para los datos del transporte SAP VTTK
         lti_vttk TYPE STANDARD TABLE OF vttk,
         les_vttk TYPE vttk,
*.....Tabla Interna para Datos de entrega
        lti_likp TYPE TABLE OF ltp_likp,
        les_likp TYPE ltp_likp,
*.....Tabla interna para los  Estados de conteo
         lti_est_conteo TYPE STANDARD TABLE OF ltp_est_conteo,
         les_est_conteo TYPE ltp_est_conteo,
*.....Tabla Interna para Datos de transporte
*{   REPLACE        ER3K900346                                        6
*\         lti_trasnporte TYPE STANDARD TABLE OF zmm_t_transporte,
*\         les_trasnporte TYPE zmm_t_transporte,
         lti_transporte TYPE STANDARD TABLE OF zmm_t_transporte,
         les_transporte TYPE zmm_t_transporte,
*}   REPLACE
*.....Variable Estructura con Información del Documento
         les_cab_dpc TYPE zesd_cab_dpc,
*.....Tabla Interna para Datos de Cliente
         lti_kna1 TYPE TABLE OF ltp_kna1,
         les_kna1 TYPE ltp_kna1,
*.....Variable Estructura para Mensajes
         les_msg TYPE zesd_msg_despacho,
*.....Rango para Fechas de Creacion de Pedidos
         rl_erdat TYPE RANGE OF  vbak-erdat,
         ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
         lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Fecha Inicio
         l_e_fecha_inicio TYPE d,
*{   INSERT         ER3K900346                                        2
         lv_tknum_se TYPE ZED_TKNUM,
*}   INSERT
         lv_tknum TYPE tknum,
         lv_ebeln TYPE ebeln,
         lv_kunnr TYPE kunnr,
*.....Fecha Final
         l_e_fecha_fin TYPE d.

  FIELD-SYMBOLS:        <lfs_kna1> TYPE ltp_kna1,
                 <lfs_likp> TYPE ltp_likp,
                <lfs_cab_dpc> TYPE zesd_cab_dpc.
*....  rangos para filtrar la tabla de estado de conteo
  DATA: rl_documento TYPE RANGE OF zmm_t_transporte-documento,  "range table
      les_documento LIKE LINE OF rl_documento.     "work area for range table


*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PACKING'
      cod_modulo      = i_tipo_documento        "DESP
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.
*.....Verifico que no Existan Mensajes de Error
  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido que exista Información para el Usuario
    IF detalles_usuario IS NOT INITIAL.
*.... Si es busqueda por transporte
      IF i_documento_origen IS NOT INITIAL AND i_cod_cliente IS INITIAL.
*{   REPLACE        ER3K900346                                        1
*\*      MOVE i_documento_origen TO lv_tknum.
*\*      MOVE i_documento_origen TO lv_EBELN.
*\*.....Función para Ajuste de Número
*\        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\          EXPORTING
*\            input  = i_documento_origen
*\          IMPORTING
*\            output = lv_tknum.
*\
*\*.... Busco el transporte
*\        SELECT SINGLE  *
*\          INTO les_trasnporte
*\          FROM zmm_t_transporte
*\          WHERE tknum EQ  lv_tknum
*\          AND   estado NE 'CERRADO'.
*\
*\        IF sy-subrc EQ 0.
*\*          APPEND les_trasnporte TO lti_trasnporte.
*\
*\
*\*.....Función para Ajuste de Número
*\          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\            EXPORTING
*\              input  = les_trasnporte-documento
*\            IMPORTING
*\              output = lv_ebeln.
*\*.... Consulto las entregas
*\          SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
*\                             FROM  likp
*\                             INTO TABLE lti_likp
*\                             WHERE vbeln EQ lv_ebeln.
*\*....Validar el estado de conteo para los documentos pendientes por despacho
*\          SELECT numero_doc
*\          INTO TABLE lti_est_conteo
*\          FROM zmm_t_est_conteo WHERE numero_doc = lv_ebeln
*\          AND proceso = 'PACKING' AND estado = 'TERMINADO'.
*\          IF sy-subrc EQ 0.
*\            CLEAR:rl_documento[].
*\            LOOP AT lti_est_conteo INTO les_est_conteo.
*\              CLEAR:les_documento.
*\              les_documento-sign   = 'I'.   "I = include, E = exclude
*\              les_documento-option = 'EQ'.  "EQ, BT, NE ....
*\*.....Función para Ajuste de Número
*\              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\                EXPORTING
*\                  input  = les_est_conteo-numero_doc
*\                IMPORTING
*\                  output = les_documento-low.
*\              APPEND les_documento TO rl_documento.
*\              les_documento-low = les_est_conteo-numero_doc.
*\              APPEND les_documento TO rl_documento.
*\            ENDLOOP.
*\            IF rl_documento IS NOT INITIAL.
*\*   Consulto los transportes encontrados en la tabla de estado.
*\              SELECT *
*\                INTO CORRESPONDING FIELDS OF TABLE lti_trasnporte
*\                FROM zmm_t_transporte
*\                WHERE documento IN rl_documento
*\                AND tipo_doc EQ i_tipo_documento.
*\            ENDIF.
*\
*\          ELSE.
*\            les_msg-num_doc = i_documento_origen.
*\            les_msg-msg = 'Transporte no esta pendiente por despacho'.
*\            les_msg-type_msg = 'E'.
*\            APPEND les_msg TO c_mensajes.
*\            RETURN.
*\          ENDIF.
*\        ELSE.
*\          les_msg-num_doc = i_documento_origen.
*\          les_msg-msg = 'Transporte no esta pendiente por despacho'.
*\          les_msg-type_msg = 'E'.
*\          APPEND les_msg TO c_mensajes.
*\          RETURN.
*\        ENDIF.
*      MOVE i_documento_origen TO lv_tknum.
*      MOVE i_documento_origen TO lv_EBELN.
        CASE i_tipo_documento.
          WHEN 'DPEN'.
*        .....Función para Ajuste de Número
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = i_documento_origen
                  IMPORTING
                    output = lv_tknum.

*        .... Busco el transporte
                SELECT SINGLE  MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                  INTO les_transporte
                  FROM zmm_t_transporte
                  WHERE tknum EQ  lv_tknum
                  AND   estado NE 'CERRADO'.

                IF sy-subrc EQ 0.
*                  APPEND les_trasnporte TO lti_trasnporte.


*        .....Función para Ajuste de Número
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = les_transporte-documento
                    IMPORTING
                      output = lv_ebeln.
*        .... Consulto las entregas
                  SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                                     FROM  likp
                                     INTO TABLE lti_likp
                                     WHERE vbeln EQ lv_ebeln.
*        ....Validar el estado de conteo para los documentos pendientes por despacho
                  SELECT numero_doc
                  INTO TABLE lti_est_conteo
                  FROM zmm_t_est_conteo WHERE numero_doc = lv_ebeln
                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.
                  IF sy-subrc EQ 0.
                    CLEAR:rl_documento[].
                    LOOP AT lti_est_conteo INTO les_est_conteo.
                      CLEAR:les_documento.
                      les_documento-sign   = 'I'.   "I = include, E = exclude
                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
*        .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = les_est_conteo-numero_doc
                        IMPORTING
                          output = les_documento-low.
                      APPEND les_documento TO rl_documento.
                      les_documento-low = les_est_conteo-numero_doc.
                      APPEND les_documento TO rl_documento.
                    ENDLOOP.
                    IF rl_documento IS NOT INITIAL.
*           Consulto los transportes encontrados en la tabla de estado.
                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                        FROM zmm_t_transporte
                        WHERE documento IN rl_documento.
*                        AND tipo_doc EQ i_tipo_documento.
                    ENDIF.

                  ELSE.
                    les_msg-num_doc = i_documento_origen.
                    les_msg-msg = 'Transporte no esta pendiente por despacho'.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    RETURN.
                  ENDIF.
                ELSE.
                  les_msg-num_doc = i_documento_origen.
                  les_msg-msg = 'Transporte no esta pendiente por despacho'.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                  RETURN.
                ENDIF.
          WHEN 'DPSE'.
*        .....Función para Ajuste de Número
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = i_documento_origen
                  IMPORTING
                    output = lv_tknum.

*           Consulto los transportes encontrados en la tabla de estado.
                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                        FROM zmm_t_transporte
                        WHERE tknum EQ  lv_tknum
                  AND   estado NE 'CERRADO'.

                 IF sy-subrc ne 0.
                    les_msg-num_doc = i_documento_origen.
                    les_msg-msg = 'Transporte no esta pendiente por despacho'.
                    les_msg-type_msg = 'E'.
                    APPEND les_msg TO c_mensajes.
                    RETURN.
                 ELSE.

*.... Consulto las entregas
             SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                                     FROM  likp
                                     INTO TABLE lti_likp
                                      FOR ALL ENTRIES IN lti_transporte
                                      WHERE vbeln EQ  lti_transporte-vbeln.


                 ENDIF.
          WHEN OTHERS.
        ENDCASE.

*}   REPLACE

      ELSEIF i_cod_cliente IS NOT INITIAL.

        MOVE i_cod_cliente TO lv_kunnr.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr
          IMPORTING
            output = lv_kunnr.
*{   REPLACE        ER3K900346                                        3
*\*.... Consulto las entregas
*\        SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
*\                           FROM  likp
*\                           INTO TABLE lti_likp
*\*                      WHERE werks EQ detalles_usuario-centro AND
*\*                            lgort EQ detalles_usuario-almacen AND
*\                           WHERE kunnr EQ lv_kunnr.
*\        IF sy-subrc EQ 0.
*\*....Validar el estado de conteo para los documentos pendientes por trasnporte
*\          SELECT numero_doc
*\          INTO TABLE lti_est_conteo
*\          FROM zmm_t_est_conteo
*\          FOR ALL ENTRIES IN  lti_likp WHERE numero_doc = lti_likp-vbeln
*\          AND proceso = 'PACKING' AND estado = 'TERMINADO'.
*\
*\          IF sy-subrc EQ 0.
*\            CLEAR:rl_documento[].
*\            LOOP AT lti_est_conteo INTO les_est_conteo.
*\              CLEAR:les_documento.
*\              les_documento-sign   = 'I'.   "I = include, E = exclude
*\              les_documento-option = 'EQ'.  "EQ, BT, NE ....
*\*.....Función para Ajuste de Número
*\              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\                EXPORTING
*\                  input  = les_est_conteo-numero_doc
*\                IMPORTING
*\                  output = les_documento-low.
*\              APPEND les_documento TO rl_documento.
*\              les_documento-low = les_est_conteo-numero_doc.
*\              APPEND les_documento TO rl_documento.
*\            ENDLOOP.
*\            IF rl_documento IS NOT INITIAL.
*\*   Consulto los transportes encontrados en la tabla de estado.
*\              SELECT *
*\                INTO CORRESPONDING FIELDS OF TABLE lti_trasnporte
*\                FROM zmm_t_transporte
*\                WHERE documento IN rl_documento
*\                AND tipo_doc EQ i_tipo_documento.
*\            ENDIF.
*\
*\          ENDIF.
*\        ELSE.
*\          les_msg-num_doc = i_documento_origen.
*\          les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
*\          les_msg-type_msg = 'E'.
*\          APPEND les_msg TO c_mensajes.
*\          RETURN.
*\        ENDIF.
        CASE i_tipo_documento.
          WHEN 'DPEN'.
*        .... Consulto las entregas
                SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                                   FROM  likp
                                   INTO TABLE lti_likp
*                              WHERE werks EQ detalles_usuario-centro AND
*                                    lgort EQ detalles_usuario-almacen AND
                                   WHERE kunnr EQ lv_kunnr.
                IF sy-subrc EQ 0.
*        ....Validar el estado de conteo para los documentos pendientes por trasnporte
                  SELECT numero_doc
                  INTO TABLE lti_est_conteo
                  FROM zmm_t_est_conteo
                  FOR ALL ENTRIES IN  lti_likp WHERE numero_doc = lti_likp-vbeln
                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.

                  IF sy-subrc EQ 0.
                    CLEAR:rl_documento[].
                    LOOP AT lti_est_conteo INTO les_est_conteo.
                      CLEAR:les_documento.
                      les_documento-sign   = 'I'.   "I = include, E = exclude
                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
*        .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = les_est_conteo-numero_doc
                        IMPORTING
                          output = les_documento-low.
                      APPEND les_documento TO rl_documento.
                      les_documento-low = les_est_conteo-numero_doc.
                      APPEND les_documento TO rl_documento.
                    ENDLOOP.
                    IF rl_documento IS NOT INITIAL.
*           Consulto los transportes encontrados en la tabla de estado.
                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                        FROM zmm_t_transporte
                        WHERE documento IN rl_documento
                        AND tipo_doc EQ i_tipo_documento.
                    ENDIF.

                  ENDIF.
                ELSE.
                  les_msg-num_doc = i_documento_origen.
                  les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                  RETURN.
                ENDIF.
          WHEN 'DPSE'.
*.....Consulto las Reservas para las Ordenes
          SELECT rsnum wempf kunnr aufnr
              FROM rkpf
                INTO TABLE lti_rkpf
                      WHERE kunnr EQ lv_kunnr.
              IF sy-subrc eq 0.
*        ....Validar el estado de conteo para los documentos pendientes por trasnporte
                  SELECT numero_doc
                  INTO TABLE lti_est_conteo
                  FROM zmm_t_est_conteo
                  FOR ALL ENTRIES IN  lti_rkpf WHERE numero_doc = lti_rkpf-rsnum
                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.

                  IF sy-subrc EQ 0.
                    CLEAR:rl_documento[].
                    LOOP AT lti_est_conteo INTO les_est_conteo.
                      CLEAR:les_documento.
                      les_documento-sign   = 'I'.   "I = include, E = exclude
                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
*        .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = les_est_conteo-numero_doc
                        IMPORTING
                          output = les_documento-low.
                      APPEND les_documento TO rl_documento.
                      les_documento-low = les_est_conteo-numero_doc.
                      APPEND les_documento TO rl_documento.
                    ENDLOOP.
                    IF rl_documento IS NOT INITIAL.
*           Consulto los transportes encontrados en la tabla de estado.
                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                        FROM zmm_t_transporte
                        WHERE documento IN rl_documento
                        AND tipo_doc EQ i_tipo_documento.
                    ENDIF.

                  ENDIF.
              ELSE.
                  les_msg-num_doc = i_documento_origen.
                  les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                  RETURN.
              ENDIF.

          WHEN OTHERS.
**.....Consulto las Reservas para las Ordenes
*          SELECT rsnum wempf kunnr aufnr
*              FROM rkpf
*                INTO TABLE lti_rkpf
*                      WHERE kunnr EQ lv_kunnr.
*              IF sy-subrc eq 0.
**        ....Validar el estado de conteo para los documentos pendientes por trasnporte
*                  SELECT numero_doc
*                  INTO TABLE lti_est_conteo
*                  FROM zmm_t_est_conteo
*                  FOR ALL ENTRIES IN  lti_rkpf WHERE numero_doc = lti_rkpf-aufnr
*                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.
*
*                  IF sy-subrc EQ 0.
*                    CLEAR:rl_documento[].
*                    LOOP AT lti_est_conteo INTO les_est_conteo.
*                      CLEAR:les_documento.
*                      les_documento-sign   = 'I'.   "I = include, E = exclude
*                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
**        .....Función para Ajuste de Número
*                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                        EXPORTING
*                          input  = les_est_conteo-numero_doc
*                        IMPORTING
*                          output = les_documento-low.
*                      APPEND les_documento TO rl_documento.
*                      les_documento-low = les_est_conteo-numero_doc.
*                      APPEND les_documento TO rl_documento.
*                    ENDLOOP.
*                    IF rl_documento IS NOT INITIAL.
**           Consulto los transportes encontrados en la tabla de estado.
*                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
*                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
*                        FROM zmm_t_transporte
*                        WHERE documento IN rl_documento
*                        AND tipo_doc EQ i_tipo_documento.
*                    ENDIF.
*
*                  ENDIF.
*              ELSE.
*                  les_msg-num_doc = i_documento_origen.
*                  les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
*                  les_msg-type_msg = 'E'.
*                  APPEND les_msg TO c_mensajes.
*                  RETURN.
*              ENDIF.

        ENDCASE.

*}   REPLACE
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

*{   REPLACE        ER3K900346                                        7
*\        SELECT *
*\          INTO CORRESPONDING FIELDS OF TABLE lti_packing
*\          FROM zmm_t_packing
*\          WHERE fecha IN rl_erdat.
*\
*\        IF sy-subrc EQ 0 .
*\          SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
*\                 FROM  likp
*\
*\                 INTO TABLE lti_likp
*\            FOR ALL ENTRIES IN lti_packing
*\*                      WHERE werks EQ detalles_usuario-centro AND
*\*                            lgort EQ detalles_usuario-almacen AND
*\                 WHERE vbeln EQ lti_packing-vbeln2.
*\
*\          IF sy-subrc EQ 0.
*\*....Validar el estado de conteo para los documentos pendientes por trasnporte
*\            SELECT numero_doc
*\            INTO TABLE lti_est_conteo
*\            FROM zmm_t_est_conteo
*\            FOR ALL ENTRIES IN  lti_likp WHERE numero_doc = lti_likp-vbeln
*\            AND proceso = 'PACKING' AND estado = 'TERMINADO'.
*\
*\
*\            IF sy-subrc EQ 0.
*\              CLEAR:rl_documento[].
*\              LOOP AT lti_est_conteo INTO les_est_conteo.
*\                CLEAR:les_documento.
*\                les_documento-sign   = 'I'.   "I = include, E = exclude
*\                les_documento-option = 'EQ'.  "EQ, BT, NE ....
*\*.....Función para Ajuste de Número
*\                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\                  EXPORTING
*\                    input  = les_est_conteo-numero_doc
*\                  IMPORTING
*\                    output = les_documento-low.
*\                APPEND les_documento TO rl_documento.
*\                les_documento-low = les_est_conteo-numero_doc.
*\                APPEND les_documento TO rl_documento.
*\              ENDLOOP.
*\              IF rl_documento IS NOT INITIAL.
*\*   Consulto los transportes encontrados en la tabla de estado.
*\                SELECT *
*\                  INTO CORRESPONDING FIELDS OF TABLE lti_trasnporte
*\                  FROM zmm_t_transporte
*\                  WHERE documento IN rl_documento
*\                  AND tipo_doc EQ i_tipo_documento.
*\              ENDIF.
*\
*\            ENDIF.
*\
*\
*\          ENDIF.
*\
*\        ENDIF.
        SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA  HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
          INTO CORRESPONDING FIELDS OF TABLE lti_packing
          FROM zmm_t_packing
          WHERE fecha IN rl_erdat.

        CASE i_tipo_documento.
          WHEN 'DPEN'.
                IF lti_packing is not INITIAL .
                  SELECT vbeln ernam erzet erdat bzirk vstel vkorg lfart autlf lifnr kunnr
                         FROM  likp

                         INTO TABLE lti_likp
                    FOR ALL ENTRIES IN lti_packing
*                              WHERE werks EQ detalles_usuario-centro AND
*                                    lgort EQ detalles_usuario-almacen AND
                         WHERE vbeln EQ lti_packing-vbeln2.

                  IF sy-subrc EQ 0.
*        ....Validar el estado de conteo para los documentos pendientes por trasnporte
                    SELECT numero_doc
                    INTO TABLE lti_est_conteo
                    FROM zmm_t_est_conteo
                    FOR ALL ENTRIES IN  lti_likp WHERE numero_doc = lti_likp-vbeln
                    AND proceso = 'PACKING' AND estado = 'TERMINADO'.
                    IF sy-subrc EQ 0.
                      CLEAR:rl_documento[].
                      LOOP AT lti_est_conteo INTO les_est_conteo.
                        CLEAR:les_documento.
                        les_documento-sign   = 'I'.   "I = include, E = exclude
                        les_documento-option = 'EQ'.  "EQ, BT, NE ....
*        .....Función para Ajuste de Número
                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                          EXPORTING
                            input  = les_est_conteo-numero_doc
                          IMPORTING
                            output = les_documento-low.
                        APPEND les_documento TO rl_documento.
                        les_documento-low = les_est_conteo-numero_doc.
                        APPEND les_documento TO rl_documento.
                      ENDLOOP.
                      IF rl_documento IS NOT INITIAL.
*           Consulto los transportes encontrados en la tabla de estado.
                        SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                          INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                          FROM zmm_t_transporte
                          WHERE documento IN rl_documento
                          AND tipo_doc EQ i_tipo_documento.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                ENDIF.
          WHEN 'DPSE'.
                IF lti_packing is not INITIAL .
*.....Consulto las Reservas para las Ordenes
          SELECT rsnum wempf kunnr aufnr
              FROM rkpf
                INTO TABLE lti_rkpf
                 FOR ALL ENTRIES IN lti_packing
                      WHERE rsnum EQ lti_packing-rsnum.
              IF sy-subrc eq 0.
*        ....Validar el estado de conteo para los documentos pendientes por trasnporte
                  SELECT numero_doc
                  INTO TABLE lti_est_conteo
                  FROM zmm_t_est_conteo
                  FOR ALL ENTRIES IN  lti_rkpf WHERE numero_doc = lti_rkpf-rsnum
                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.

                  IF sy-subrc EQ 0.
                    CLEAR:rl_documento[].
                    LOOP AT lti_est_conteo INTO les_est_conteo.
                      CLEAR:les_documento.
                      les_documento-sign   = 'I'.   "I = include, E = exclude
                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
*        .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = les_est_conteo-numero_doc
                        IMPORTING
                          output = les_documento-low.
                      APPEND les_documento TO rl_documento.
                      les_documento-low = les_est_conteo-numero_doc.
                      APPEND les_documento TO rl_documento.
                    ENDLOOP.
                    IF rl_documento IS NOT INITIAL.
*           Consulto los transportes encontrados en la tabla de estado.
                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
                        FROM zmm_t_transporte
                        WHERE documento IN rl_documento
                        AND tipo_doc EQ i_tipo_documento.
                    ENDIF.

                  ENDIF.
              ELSE.
                  les_msg-num_doc = i_documento_origen.
                  les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                  RETURN.
              ENDIF.
                ENDIF.
          WHEN OTHERS.
*            IF lti_packing is not INITIAL .
**.....Consulto las Reservas para las Ordenes
*          SELECT rsnum wempf kunnr aufnr
*              FROM rkpf
*                INTO TABLE lti_rkpf
*                 FOR ALL ENTRIES IN lti_packing
*                      WHERE aufnr EQ lti_packing-aufnr.
*              IF sy-subrc eq 0.
**        ....Validar el estado de conteo para los documentos pendientes por trasnporte
*                  SELECT numero_doc
*                  INTO TABLE lti_est_conteo
*                  FROM zmm_t_est_conteo
*                  FOR ALL ENTRIES IN  lti_rkpf WHERE numero_doc = lti_rkpf-aufnr
*                  AND proceso = 'PACKING' AND estado = 'TERMINADO'.
*
*                  IF sy-subrc EQ 0.
*                    CLEAR:rl_documento[].
*                    LOOP AT lti_est_conteo INTO les_est_conteo.
*                      CLEAR:les_documento.
*                      les_documento-sign   = 'I'.   "I = include, E = exclude
*                      les_documento-option = 'EQ'.  "EQ, BT, NE ....
**        .....Función para Ajuste de Número
*                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                        EXPORTING
*                          input  = les_est_conteo-numero_doc
*                        IMPORTING
*                          output = les_documento-low.
*                      APPEND les_documento TO rl_documento.
*                      les_documento-low = les_est_conteo-numero_doc.
*                      APPEND les_documento TO rl_documento.
*                    ENDLOOP.
*                    IF rl_documento IS NOT INITIAL.
**           Consulto los transportes encontrados en la tabla de estado.
*                      SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
*                        INTO CORRESPONDING FIELDS OF TABLE lti_transporte
*                        FROM zmm_t_transporte
*                        WHERE documento IN rl_documento
*                        AND tipo_doc EQ i_tipo_documento.
*                    ENDIF.
*
*                  ENDIF.
*              ELSE.
*                  les_msg-num_doc = i_documento_origen.
*                  les_msg-msg = 'cliente no tiene transportes pendientes por despacho'.
*                  les_msg-type_msg = 'E'.
*                  APPEND les_msg TO c_mensajes.
*                  RETURN.
*              ENDIF.
*                ENDIF.
        ENDCASE.
*}   REPLACE
      ENDIF.
*{   REPLACE        ER3K900346                                        8
*\      IF lti_trasnporte IS NOT INITIAL .
*\
*\*.... Consulto la información de la cabecera de los transportes
*\        SELECT *
*\          INTO CORRESPONDING FIELDS OF TABLE lti_vttk
*\          FROM vttk
*\          FOR ALL ENTRIES IN lti_trasnporte
*\          WHERE tknum EQ lti_trasnporte-tknum.
*\
*\*.....Consulto Informacion del Cliente
*\        SELECT kunnr name1
*\              FROM  kna1
*\               INTO TABLE lti_kna1
*\                  FOR ALL ENTRIES IN lti_likp
*\                      WHERE kunnr EQ lti_likp-kunnr.
*\
*\*... Recorro la información de los trasnportes para realizar el despacho
*\        LOOP AT lti_trasnporte INTO les_trasnporte.
*\          CLEAR: les_cab_dpc.
*\*... leo la cabecera de la entrega
*\          READ TABLE lti_likp WITH KEY vbeln = les_trasnporte-documento(10)
*\                                 ASSIGNING <lfs_likp>.
*\          IF <lfs_likp> IS ASSIGNED.
*\*... leo la cabecera del cliente
*\            READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
*\                              ASSIGNING <lfs_kna1>.
*\            IF <lfs_kna1> IS ASSIGNED.
*\*... Asigno codigo y nombre del cliente
*\              les_cab_dpc-cod_cliente = <lfs_likp>-kunnr.
*\              les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
*\            ENDIF.
*\          ELSE.
*\            READ TABLE lti_likp WITH KEY vbeln = les_trasnporte-documento+2(10)
*\                                 ASSIGNING <lfs_likp>.
*\            IF <lfs_likp> IS ASSIGNED.
*\*... leo la cabecera del cliente
*\              READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
*\                                ASSIGNING <lfs_kna1>.
*\              IF <lfs_kna1> IS ASSIGNED.
*\*... Asigno codigo y nombre del cliente
*\                les_cab_dpc-cod_cliente = <lfs_likp>-kunnr.
*\                les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
*\              ENDIF.
*\
*\            ENDIF.
*\
*\          ENDIF.
*\
*\*leo la cabecera del transporte sap
*\          READ TABLE  lti_vttk INTO les_vttk WITH KEY tknum = les_trasnporte-tknum.
*\          IF sy-subrc EQ 0.
*\            MOVE les_vttk-tdlnr TO lv_kunnr.
*\*.....Función para Ajuste de Número
*\            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\              EXPORTING
*\                input  = lv_kunnr
*\              IMPORTING
*\                output = lv_kunnr.
*\*... Consulto el nombre del transportista
*\            SELECT SINGLE name1
*\              FROM  kna1
*\               INTO les_cab_dpc-transportista
*\                    WHERE kunnr EQ lv_kunnr.
*\*            LES_CAB_DPC-TRANSPORTISTA =
*\            les_cab_dpc-ruta = les_vttk-route.
*\          ENDIF.
*\          les_cab_dpc-num_doc = les_trasnporte-tknum.
*\          les_cab_dpc-fecha_crea = les_trasnporte-fecha.
*\          les_cab_dpc-tipo_doc = i_tipo_documento.
*\
*\          CASE i_tipo_documento.
*\            WHEN 'DPEN'.
*\              les_cab_dpc-vbeln = les_trasnporte-documento.
*\            WHEN 'DPSE'.
*\            WHEN OTHERS.
*\              les_cab_dpc-vbeln = les_trasnporte-documento.
*\          ENDCASE.
*\          APPEND les_cab_dpc TO c_det_cabecera.
*\        ENDLOOP.
*\      ELSE.
*\        les_msg-msg = 'No Existen Documentos Pendientes por Despacho'.
*\        les_msg-type_msg = 'E'.
*\        APPEND les_msg TO c_mensajes.
*\        RETURN.
*\      ENDIF.
      IF lti_transporte IS NOT INITIAL .

          CASE i_tipo_documento.
            WHEN 'DPEN'.
*          .... Consulto la información de la cabecera de los transportes
                  SELECT tknum tdlnr ROUTE
                    INTO CORRESPONDING FIELDS OF TABLE lti_vttk
                    FROM vttk
                    FOR ALL ENTRIES IN lti_transporte
                    WHERE tknum EQ lti_transporte-tknum.

*          .....Consulto Informacion del Cliente
                  SELECT kunnr name1
                        FROM  kna1
                         INTO TABLE lti_kna1
                            FOR ALL ENTRIES IN lti_likp
                                WHERE kunnr EQ lti_likp-kunnr.

*          ... Recorro la información de los trasnportes para realizar el despacho
                  LOOP AT lti_transporte INTO les_transporte.
                    CLEAR: les_cab_dpc.
*          ... leo la cabecera de la entrega
                    READ TABLE lti_likp WITH KEY vbeln = les_transporte-documento(10)
                                           ASSIGNING <lfs_likp>.
                    IF <lfs_likp> IS ASSIGNED.
*          ... leo la cabecera del cliente
                      READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                        ASSIGNING <lfs_kna1>.
                      IF <lfs_kna1> IS ASSIGNED.
*          ... Asigno codigo y nombre del cliente
                        les_cab_dpc-cod_cliente = <lfs_likp>-kunnr.
                        les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
                      ENDIF.
                    ELSE.
                      READ TABLE lti_likp WITH KEY vbeln = les_transporte-documento+2(10)
                                           ASSIGNING <lfs_likp>.
                      IF <lfs_likp> IS ASSIGNED.
*          ... leo la cabecera del cliente
                        READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                          ASSIGNING <lfs_kna1>.
                        IF <lfs_kna1> IS ASSIGNED.
*          ... Asigno codigo y nombre del cliente
                          les_cab_dpc-cod_cliente = <lfs_likp>-kunnr.
                          les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
                        ENDIF.

                      ENDIF.

                    ENDIF.

*          leo la cabecera del transporte sap
                    READ TABLE  lti_vttk INTO les_vttk WITH KEY tknum = les_transporte-tknum.
                    IF sy-subrc EQ 0.
                      MOVE les_vttk-tdlnr TO lv_kunnr.
*          .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lv_kunnr
                        IMPORTING
                          output = lv_kunnr.
*          ... Consulto el nombre del transportista
                      SELECT SINGLE name1
                        FROM  kna1
                         INTO les_cab_dpc-transportista
                              WHERE kunnr EQ lv_kunnr.
*                      LES_CAB_DPC-TRANSPORTISTA =
                      les_cab_dpc-ruta = les_vttk-route.
                    ENDIF.
                    les_cab_dpc-num_doc = les_transporte-tknum.
                    les_cab_dpc-fecha_crea = les_transporte-fecha.
                    les_cab_dpc-tipo_doc = i_tipo_documento.

                    CASE i_tipo_documento.
                      WHEN 'DPEN'.
                        les_cab_dpc-vbeln = les_transporte-documento.
                      WHEN 'DPSE'.
                      WHEN OTHERS.
                        les_cab_dpc-vbeln = les_transporte-documento.
                    ENDCASE.
                    APPEND les_cab_dpc TO c_det_cabecera.
                  ENDLOOP.
            WHEN 'DPSE'.
*.... Consulto la información de la cabecera de los transportes
                  SELECT tknum tdlnr ROUTE
                    INTO CORRESPONDING FIELDS OF TABLE lti_vttk
                    FROM vttk
                    FOR ALL ENTRIES IN lti_transporte
                    WHERE tknum EQ lti_transporte-tknum.

*          .....Consulto Informacion del Cliente
                  SELECT kunnr name1
                        FROM  kna1
                         INTO TABLE lti_kna1
                            FOR ALL ENTRIES IN lti_likp
                                WHERE kunnr EQ lti_likp-kunnr.
*          ... Recorro la información de los trasnportes para realizar el despacho
                  LOOP AT lti_transporte INTO les_transporte.
                    CLEAR: les_cab_dpc.
*          ... leo la cabecera de la entrega
                    READ TABLE lti_likp WITH KEY vbeln = les_transporte-vbeln
                                           ASSIGNING <lfs_likp>.
                    IF sy-subrc eq 0.
*          ... leo la cabecera del cliente
                      READ TABLE lti_kna1 WITH KEY kunnr = <lfs_likp>-kunnr
                                        ASSIGNING <lfs_kna1>.
                      IF <lfs_kna1> IS ASSIGNED.
*          ... Asigno codigo y nombre del cliente
                        les_cab_dpc-cod_cliente = <lfs_likp>-kunnr.
                        les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
                      ENDIF.

                    ELSE.
                      READ TABLE lti_rkpf WITH KEY aufnr = les_transporte-documento(10)
                                           into les_rkpf.
                      IF sy-subrc eq 0.
*              ... leo la cabecera del cliente
                          READ TABLE lti_kna1 WITH KEY kunnr = les_rkpf-kunnr
                                            ASSIGNING <lfs_kna1>.
                          IF <lfs_kna1> IS ASSIGNED.
*              ... Asigno codigo y nombre del cliente
                            les_cab_dpc-cod_cliente = les_rkpf-kunnr.
                            les_cab_dpc-nom_cliente = <lfs_kna1>-name1.
                          ENDIF.

                      ENDIF.
                    ENDIF.
*          leo la cabecera del transporte sap
                    READ TABLE  lti_vttk INTO les_vttk WITH KEY tknum = les_transporte-tknum.
                    IF sy-subrc EQ 0.
                      MOVE les_vttk-tdlnr TO lv_kunnr.
*          .....Función para Ajuste de Número
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lv_kunnr
                        IMPORTING
                          output = lv_kunnr.
*          ... Consulto el nombre del transportista
                      SELECT SINGLE name1
                        FROM  kna1
                         INTO les_cab_dpc-transportista
                              WHERE kunnr EQ lv_kunnr.
*                      LES_CAB_DPC-TRANSPORTISTA =
                      les_cab_dpc-ruta = les_vttk-route.
                    ENDIF.

                    les_cab_dpc-num_doc = les_transporte-tknum_se.
                    les_cab_dpc-fecha_crea = les_transporte-fecha.
                    les_cab_dpc-tipo_doc = i_tipo_documento.
                    les_cab_dpc-vbeln = les_transporte-documento.
                    APPEND les_cab_dpc TO c_det_cabecera.
                  ENDLOOP.
            WHEN OTHERS.
          ENDCASE.
      ELSE.
        les_msg-msg = 'No Existen Documentos Pendientes por Despacho'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ENDIF.
*}   REPLACE

    ELSE.

      les_msg-num_doc = i_documento_origen.
      les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.
      RETURN.
    ENDIF.
  ENDIF.
ENDMETHOD.


method GET_DETALLE.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle de Documentos despachos
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
 DATA :
       lti_TRANSP_DT type STANDARD TABLE OF zmm_t_TRANSP_DT,
       les_TRANSP_DT type zmm_t_TRANSP_DT,
*.....Variable Estructura para Tabla de Mensajes
           les_msg TYPE zesd_msg_despacho,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_dpc,

*.....Variable Texto Error
           l_e_desc_error TYPE string,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,

*{   INSERT         ER3K900346                                        2
          lv_tknum_se TYPE zed_tknum,
*}   INSERT
           lv_tknum TYPE tknum,
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
    if detalles_usuario is not initial.
*{   REPLACE        ER3K900346                                        1
*\      move i_documento_origen to lv_tknum.
*\*.....Función para Ajuste de Número
*\      call function 'CONVERSION_EXIT_ALPHA_INPUT'
*\        exporting
*\          input  = lv_tknum
*\        importing
*\          output = lv_tknum.
*\*.... Consulto el detalle del transporte
*\      SELECT *
*\        into CORRESPONDING FIELDS OF TABLE lti_TRANSP_DT
*\        from ZMM_T_TRANSP_DT
*\        where tknum eq lv_tknum .
*\        IF sy-subrc eq 0.
*\*.... Añado los registros a la tabla de detalles
*\          LOOP AT lti_TRANSP_DT into les_TRANSP_DT.
*\            MOVE-CORRESPONDING les_TRANSP_DT  to les_det_pos.
*\*... Convierto la Unidad de manipulación a codigo de barras
*\            CALL METHOD zcl_lgtica_embalaje=>get_rotulo
*\              EXPORTING
*\                i_exidv  = les_TRANSP_DT-exidv
*\              IMPORTING
*\                e_rotulo = les_det_pos-cod_bar
*\                .
*\
*\            APPEND les_det_pos to c_det_posiciones.
*\          ENDLOOP.
*\
*\        ENDIF.
      CASE i_tipo_documento.
        WHEN 'DPEN'.
move i_documento_origen to lv_tknum.
*.....Función para Ajuste de Número
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = lv_tknum
        importing
          output = lv_tknum.
*.... Consulto el detalle del transporte
      SELECT MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
        into TABLE lti_TRANSP_DT
        from ZMM_T_TRANSP_DT
        where tknum eq lv_tknum .
        IF sy-subrc eq 0.
*.... Añado los registros a la tabla de detalles
          LOOP AT lti_TRANSP_DT into les_TRANSP_DT.
            MOVE-CORRESPONDING les_TRANSP_DT  to les_det_pos.
*... Convierto la Unidad de manipulación a codigo de barras
            CALL METHOD zcl_lgtica_embalaje=>get_rotulo
              EXPORTING
                i_exidv  = les_TRANSP_DT-exidv
              IMPORTING
                e_rotulo = les_det_pos-cod_bar
                .

            APPEND les_det_pos to c_det_posiciones.
          ENDLOOP.

        ENDIF.
        WHEN 'DPSE'.
          move i_documento_origen to lv_tknum.
*.....Función para Ajuste de Número
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = lv_tknum
        importing
          output = lv_tknum.
*.... Consulto el detalle del transporte
      SELECT MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
        into TABLE lti_TRANSP_DT
        from ZMM_T_TRANSP_DT
        where tknum eq lv_tknum .
        IF sy-subrc eq 0.
*.... Añado los registros a la tabla de detalles
          LOOP AT lti_TRANSP_DT into les_TRANSP_DT.
            MOVE-CORRESPONDING les_TRANSP_DT  to les_det_pos.
*... Convierto la Unidad de manipulación a codigo de barras

                  CONCATENATE les_TRANSP_DT-TKNUM les_TRANSP_DT-consecutivo INTO les_det_pos-cod_bar.

            APPEND les_det_pos to c_det_posiciones.
          ENDLOOP.

        ENDIF.
        WHEN OTHERS.
      ENDCASE.

*}   REPLACE
    ENDIF.
  ENDIF.
endmethod.


method GET_DETALLES_USUARIO.
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
endmethod.


METHOD guardar_despacho.

  DATA: les_detail    TYPE zesd_det_dpc,
        les_detail_t  TYPE zesd_det_dpc,
        lti_detail    TYPE TABLE OF zesd_det_dpc,
        lti_transp_dt TYPE TABLE OF zmm_t_transp_dt,
        les_transp_dt TYPE zmm_t_transp_dt.

  READ TABLE c_detail INTO les_detail INDEX 1.
*  Consultar datos en base de datos.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lti_detail
    FROM zmm_t_transp_dt
    WHERE tknum EQ les_detail-tknum.

  CLEAR les_detail.
*  Recorrer el detalle e identificar cuales ya se han despachado.
  LOOP AT c_detail INTO les_detail.
    READ TABLE lti_detail INTO les_detail_t WITH KEY venum = les_detail-venum.
    IF les_detail_t-estado EQ 'DS' OR les_detail_t-estado EQ 'OK'.
      les_detail-estado = 'DS'.
    ENDIF.
*    Actualizar usuario.
    les_detail-usuario = i_usuario.
*    Modificar los estados.
    MODIFY c_detail FROM les_detail.
  ENDLOOP.

*  Mapear datos a guardar en BD.
  LOOP AT c_detail INTO les_detail.
    MOVE-CORRESPONDING les_detail to les_transp_dt.
*{   INSERT         ER3K900332                                        1
    les_transp_dt-tknum_se = les_detail-tknum.
*}   INSERT
    APPEND les_transp_dt TO lti_transp_dt.
  ENDLOOP.

*  Actualizar datos en base de datos.
  CALL METHOD zcl_lgtica_util=>modify_zmm_t_transp_dt
    EXPORTING
      i_ti_transp_dt = lti_transp_dt
      i_mod          = 'M'.


ENDMETHOD.
ENDCLASS.
