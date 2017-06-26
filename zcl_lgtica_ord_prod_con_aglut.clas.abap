class ZCL_LGTICA_ORD_PROD_CON_AGLUT definition
  public
  inheriting from ZCL_LGTICA_DOCUMENTO
  final
  create public .

*"* public components of class ZCL_LGTICA_ORD_PROD_CON_AGLUT
*"* do not include other source files here!!!
public section.

  methods ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_CABECERA
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~GET_DETALLE
    redefinition .
  methods ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_HEADER_PICKING
    redefinition .
  methods ZIF_LGTICA_PICKABLE~GET_POS_PICKING
    redefinition .
protected section.
*"* protected components of class ZCL_LGTICA_ORD_PROD_SIN_AGLUT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_ORD_PROD_SIN_AGLUT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_ORD_PROD_CON_AGLUT IMPLEMENTATION.


method ZIF_LGTICA_DOCUMENTO~GENERAR_ENTREGAS.
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

endmethod.


METHOD zif_lgtica_documento~get_cabecera.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Cabecera de Documentos para Ordenes de Prod. Con Aglutinador
* Autor Prog.  :
* Fecha Creac. : 14.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
*
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
*{   INSERT         ER3K900279                                        1
*... tipo para el manejo de los status de equipos
  TYPES: BEGIN OF ltp_jest,
      OBJNR type jest-OBJNR,
      STAT type jest-STAT,
      INACT type jest-INACT,
    END OF ltp_jest.
*... tipo para el manejo de los equipos
  TYPES: BEGIN OF ltp_equi,
        MATNR type equi-MATNR,
        WERKS type equi-werk,
        LGORT type equi-lager,
        SERNR type equi-SERNR,
        equnr type equi-equnr,
        EQTYP type equi-EQTYP,
        objnr type equi-objnr,
       END OF ltp_equi.

*.....Typo para Datos Completos de Reservas
  TYPES: BEGIN OF ltp_reservas,
      reserv_no TYPE  rsnum,
      res_item TYPE	rspos,
      material TYPE	matnr,
      plant TYPE  werks_d,
      stge_loc TYPE	lgort_d,
      batch TYPE  charg_d,
      entry_qnt TYPE erfmg,
      entry_uom TYPE erfme,
      entry_uom_iso TYPE isocd_unit,
      req_date TYPE	bdter,
      gl_account TYPE	saknr,
      item_text	 TYPE sgtxt,
      gr_rcpt TYPE  wempf,
      unload_pt	 TYPE ablad,
      fixed_quan TYPE	fmeng,
      movement TYPE	xwaok,
      withd_quan TYPE	enmng,
      base_uom TYPE	meins,
      base_uom_iso TYPE	isocd_unit,
      process_id TYPE	beakz,
      material_external TYPE  mgv_material_external,
      material_guid TYPE  mgv_material_guid,
      material_version TYPE	mgv_material_version,
*{   INSERT         ER3K900279                                        1
     STK_SEGMENT  Type  SGT_SCAT,
     REQ_SEGMENT  Type  SGT_RCAT,
*}   INSERT
      seriales TYPE  c LENGTH 1 ,  "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
      numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
 END OF ltp_reservas.
*}   INSERT

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

*.....Typo para Datos de Reservas de Ordenes
  TYPES: BEGIN OF ltp_rkpf,
         rsnum TYPE rsnum,    "Número de la reserva/las necesidades secundarias
         wempf TYPE wempf,    "Destinatario de mercancías
         kunnr TYPE ekunn,    "Número de cuenta del cliente
         aufnr TYPE aufnr,    "Número de orden
        END OF ltp_rkpf.

*.....Typo para datos de Pedido
  TYPES: BEGIN OF ltp_vbak,
         vbeln TYPE vbeln_va, "Documento de ventas
         kunnr TYPE kunag,    "Solicitante
         aufnr TYPE aufnr,    "Número de orden
        END OF ltp_vbak.

*.....Typo para Datos de Cliente
  TYPES: BEGIN OF ltp_kna1,
         kunnr TYPE kunnr,    "Nº de cliente 1
         name1 TYPE name1_gp,                               "Nombre 1
        END OF ltp_kna1.
*.....Typo para Datos de Material para Ordenes de Produccion sin Aglutinador
  TYPES: BEGIN OF ltp_afko,
         aufnr TYPE aufnr,
         plnbez TYPE matnr,
        END OF ltp_afko.

*.....Texto Material
  TYPES: BEGIN OF ltp_makt,
         matnr TYPE matnr,
         maktx TYPE maktx,
        END OF ltp_makt.
  DATA :
*..... Tabla interna con datos de la tabla de el aglutinador y la asociación de las ordenes
        lti_zppt_0001 TYPE TABLE OF ltp_zppt_0001,
*.....Tabla Interna con Datos de Orden
        lti_afko TYPE TABLE OF ltp_afko,
*.....Tabla Interna con Datos de Material
        lti_makt TYPE TABLE OF ltp_makt,

*.....Tabla Interna con datos Basicos de Orden
        lti_aufk TYPE TABLE OF ltp_aufk,
*.....Tabla Interna con datos Basico de Reservas
        lti_rkpf TYPE TABLE OF ltp_rkpf,
*.....Tabla Interna con Datos de Pedido
        lti_vbak TYPE TABLE OF ltp_vbak,
*.....Variable Estructura con Información del Documento
        les_cab_ref TYPE zesd_cab_ref,
*.....Tabla Interna para Datos de Cliente
        lti_kna1 TYPE TABLE OF ltp_kna1,
*.....Variable Estructura para Mensajes
        les_msg TYPE zesd_msg_picking,
*.....Rango para Fechas de Creacion Ordenes de Servicio Internas (Fecha entrada)
        rl_erdat TYPE RANGE OF  aufk-erdat,
        ls_erdat LIKE LINE OF rl_erdat,
*.....Tabla Internas de Retorno al Consultar la Actividad
        lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Fecha Inicio
        l_e_fecha_inicio TYPE d,
*.....Fecha Final
        l_e_fecha_fin TYPE d,
*.....Mensaje
        l_e_msg TYPE string,
*.....Validador
        l_e_val TYPE i,
*.....Contador
        l_e_cont TYPE i,
*.... Aglutinador
        l_e_nmint TYPE zppde_0002,

*.....Variables para Bapi
        lti_bapiret2 TYPE bapiret2_tab,
*{   REPLACE        ER3K900279                                        3
*\        lti_res_items TYPE TABLE OF bapi2093_res_items_get.
        lti_res_items TYPE TABLE OF ltp_reservas.
*}   REPLACE

  FIELD-SYMBOLS: <lfs_actividad> TYPE zmm_t_clase_pv,
                 <lfs_aufk> TYPE ltp_aufk,
                 <lfs_afko> TYPE ltp_afko,
                 <lfs_makt> TYPE ltp_makt,
                 <lfs_bapiret2> TYPE bapiret2,
*{   DELETE         ER3K900279                                        4
*\                 <lfs_res_items> TYPE bapi2093_res_items_get,
*}   DELETE
                 <lfs_rkpf> TYPE ltp_rkpf,

                 <lfs_zppt_001> TYPE ltp_zppt_0001.
*{   INSERT         ER3K900279                                        2
  DATA: lti_ubicacion TYPE STANDARD TABLE OF zmm_t_ubicacion.
*... Tabla interna para consultar la tabla de ubicaciones
data: ti_zmm_t_ubicacion type standard table of zmm_t_ubicacion.
*...
data: les_ubicacion type  zmm_t_ubicacion.

data: les_ubicacion_suge type  ZESD_UBIC_SUG.
  DATA :
*..... Tabla interna con datos de la tabla de el aglutinador y la asociación de las ordenes
*        lti_zppt_0001 TYPE TABLE OF ltp_zppt_0001,
        les_zppt_0001 TYPE ltp_zppt_0001,
*.....Variable Estructura para Tabla de Mensajes
**           les_msg TYPE zesd_msg_picking,
*.....Variable Estructura para Detalle de Posiciones
           les_det_pos TYPE zesd_det_ref,
*.....Tablas de Retorno Función Busqueda Seriales y Lote para el Material
           lti_return TYPE TABLE OF bapiret1,
           lti_serno  TYPE TABLE OF zstmm_0003,
           les_serno  TYPE zstmm_0003,
           lti_equi  TYPE TABLE OF ltp_equi,
           les_equi  TYPE  ltp_equi,
           lti_jest  TYPE TABLE OF ltp_jest,
           les_jest  TYPE ltp_jest,
           lti_charg TYPE TABLE OF zstmm_0002,

           lti_estatin TYPE TABLE OF zstmm_0001,
           lti_matnr TYPE TABLE OF zstmm_0004,
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_picking,
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
*           lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string,
*.....Tabla Interna con datos Basicos de Orden
*           lti_aufk TYPE TABLE OF ltp_aufk,
*.....Tabla Interna con datos Basico de Reservas
*           lti_rkpf TYPE TABLE OF ltp_rkpf,
*.... Estructuras para buscar las reservas en la tabla resb
        lti_resb TYPE STANDARD TABLE OF resb,
        lti_resb_x TYPE STANDARD TABLE OF resb,
        les_resb TYPE resb,
        les_resb_x TYPE resb,
*.... Aglutinador
*           l_e_nmint TYPE zppde_0002,
*..... Proceso pieza
           l_e_NM_REF_h TYPE ZPPDE_0006,
*.....Mensaje
*           l_e_msg TYPE string,
*.....Variables para Bapi
  lti_res_items_x TYPE TABLE OF ltp_reservas " bapi2093_res_items_get.
.
*           lti_bapiret2 TYPE bapiret2_tab,
*           lti_res_items TYPE TABLE OF ltp_reservas. " bapi2093_res_items_get.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking,
*                  <lfs_actividad> TYPE zmm_t_clase_pv,
*                  <lfs_aufk> TYPE ltp_aufk,
*                  <lfs_rkpf> TYPE ltp_rkpf,
                  <lfs_res_items> TYPE  ltp_reservas. "bapi2093_res_items_get.

*}   INSERT

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "OPSG
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
*.....Ajusto Número de Documento
        MOVE i_documento_origen TO l_e_nmint.
*{   INSERT         ER3K900279                                        5
        MOVE i_documento_origen TO l_e_NM_REF_h.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_e_NM_REF_h
          IMPORTING
            output = l_e_NM_REF_h.

*}   INSERT
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_e_nmint
          IMPORTING
            output = l_e_nmint.


*.... Busqueda de aglutinador en la tabla   Agrupador Ordenes de producción
*{   REPLACE        ER3K900279                                       12
*\        SELECT * FROM zppt_0001
        SELECT ID NMINT TIPO NM_REF NM_REF_P NM_REF_H REF_PRO COLOR_A COLOR_B ESPEJO NIC OBSER
          FROM zppt_0001
*}   REPLACE
          INTO CORRESPONDING FIELDS OF TABLE lti_zppt_0001
*{   REPLACE        ER3K900279                                        6
*\          WHERE nmint EQ l_e_nmint
*\          ORDER BY nm_ref_p.
          WHERE nmint EQ l_e_nmint OR
          NM_REF_h EQ l_e_NM_REF_h
          order by NM_REF_h ASCENDING.
*}   REPLACE

        IF sy-subrc EQ 0.
*.....Busqueda Ordenes de Ser. para El Usuario Según el Centro y Documento Origen que tengan Asignados
          SELECT aufnr auart autyp ernam erdat werks user0 kdauf
                     FROM aufk
                      INTO TABLE lti_aufk
                        FOR ALL ENTRIES IN lti_zppt_0001
                        WHERE werks EQ detalles_usuario-centro AND
                              aufnr EQ lti_zppt_0001-nm_ref_h.

*.....Ordeno Tabla
          SORT lti_aufk BY aufnr ASCENDING.
*.....Busqueda de Datos de Material de Orden
          SELECT aufnr plnbez
              FROM afko
                 INTO TABLE lti_afko
                   FOR ALL ENTRIES IN lti_aufk
                      WHERE aufnr EQ lti_aufk-aufnr.
          IF sy-subrc EQ 0 .
*.....Ordeno Tabla
            SORT lti_afko BY plnbez ASCENDING.
*.....Busqueda de Texto de Material
            SELECT matnr maktx
               FROM makt
                  INTO TABLE lti_makt
                    FOR ALL ENTRIES IN lti_afko
                      WHERE matnr EQ lti_afko-plnbez.
          ENDIF.
        ENDIF.


      ENDIF.
*.....Verifico que Existan Ordenes
      IF lti_aufk[] IS INITIAL.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
        les_msg-type_msg = 'E'.
        APPEND les_msg TO c_mensajes.
        RETURN.
      ELSE.
*.....Se recibio como parametros de Entrada Rangos de Fecha
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
          SORT lti_aufk BY aufnr ASCENDING.
*.....Busco Información para la Clase de Documento
          READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
          IF sy-subrc EQ 0.
            READ TABLE rl_erdat INDEX 1 INTO ls_erdat.
*.....Ajusto Tabla de Ordenes segun el Rango de Fechas recibido como parametro de Entrada
            LOOP AT lti_aufk  ASSIGNING <lfs_aufk> .
              l_e_cont = sy-tabix.
              IF  <lfs_aufk>-erdat BETWEEN ls_erdat-low AND ls_erdat-high.
                CONTINUE.
              ELSE.
                DELETE lti_aufk INDEX l_e_cont.
              ENDIF.
            ENDLOOP.
*.....Limpieza de Contador
            CLEAR : l_e_cont, ls_erdat.

*......Verifico que Existan Ordenes con los Rangos de Fecha Ingresados
            IF lti_aufk[] IS INITIAL .
              les_msg-msg = 'Para el Rango de Fechas Ingresados no Existen Ordenes de Servicio en el Aglutinador'.
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
              RETURN.
            ELSE.
*.....Ordeno Tabla de Ordenes
              SORT lti_aufk BY aufnr ASCENDING.
*.....Consulto las Reservas para las Ordenes
              SELECT rsnum wempf kunnr aufnr
                  FROM rkpf
                    INTO TABLE lti_rkpf
                      FOR ALL ENTRIES IN lti_aufk
                          WHERE aufnr EQ lti_aufk-aufnr.

              IF sy-subrc EQ 0 .
                LOOP AT lti_rkpf ASSIGNING <lfs_rkpf>.
                  l_e_cont = sy-tabix.
*{   REPLACE        ER3K900279                                       17
*\                  CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
*\                    EXPORTING
*\                      reservation_no    = <lfs_rkpf>-rsnum
*\                      movement          = 'X'
*\                    TABLES
*\                      reservation_items = lti_res_items
*\                      return            = lti_bapiret2.
                  CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
                    EXPORTING
                      reservation_no    = <lfs_rkpf>-rsnum
                      movement          = 'X'
                      WITHDRAWN         = 'X'
                    TABLES
                      reservation_items = lti_res_items
                      return            = lti_bapiret2.
*}   REPLACE

*.....Lectura en Tabla de Retorno, los valores de los Campos de Key Identifican que no Esta Pend. por Picking
                  READ TABLE lti_bapiret2 WITH KEY type = 'E'
                                                   id = 'W5'
                                                   number = 027 TRANSPORTING NO FIELDS.
*.....Valido que No Existan Reservas para la Orden
                  IF sy-subrc EQ 0 .
                    DELETE lti_aufk WHERE aufnr = <lfs_rkpf>-aufnr.
                  ELSE.
*.....Ordeno Tabla para Identficar Cuales Posiciones Estan Pendientes por Picking
                    SORT lti_res_items BY movement DESCENDING.
*.....Recorro Tablas de Reserva para Identificar si existe almenos 1 Posicion pendiente por Picking
                    LOOP AT lti_res_items ASSIGNING <lfs_res_items>.
                      IF <lfs_res_items>-movement EQ 'X'.
                        EXIT.
                      ELSE.
*.... Todas las ordenes pertenecientes al aglutinador deben de mostrarse
*                        l_e_val = l_e_val + 1.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                  IF l_e_val IS NOT INITIAL.
*.....La tabla de Posiciones de Reseverva No Tiene Posiciones Pendientes por Picking
                    DELETE lti_aufk WHERE aufnr = <lfs_rkpf>-aufnr.
                  ENDIF.
                  CLEAR : lti_res_items, lti_bapiret2, l_e_val.
                ENDLOOP.
                IF lti_aufk[] IS NOT INITIAL.
*
*.....Recorro Tabla de Ordenes
                  LOOP AT lti_aufk ASSIGNING <lfs_aufk>.
                    UNASSIGN <lfs_zppt_001>.
                    READ TABLE lti_zppt_0001 ASSIGNING <lfs_zppt_001> WITH KEY nm_ref_h = <lfs_aufk>-aufnr.
                    IF <lfs_zppt_001> IS ASSIGNED.
                      les_cab_ref-aufnr =  <lfs_zppt_001>-nm_ref_p. " numero de orden proceso pieza
                    ENDIF.
                    les_cab_ref-aglutinador = l_e_nmint. " aglutinador
                    les_cab_ref-num_doc     = <lfs_aufk>-aufnr.   "Número de Orden
                    les_cab_ref-fecha_crea  = <lfs_aufk>-erdat.   "Fecha Entrada o Creación de la Orden
                    les_cab_ref-tipo_doc    = <lfs_aufk>-auart.   "Clase de Orden
*.....Lectura a Tabla de Orden para el Material
                    READ TABLE lti_afko WITH KEY aufnr = <lfs_aufk>-aufnr ASSIGNING <lfs_afko>.
                    IF sy-subrc EQ 0.
                      les_cab_ref-cod_cliente = <lfs_afko>-plnbez.   "Número de material
*.....Lectura a Tabla de Texto de Material
                      READ TABLE lti_makt WITH  KEY matnr = <lfs_afko>-plnbez ASSIGNING <lfs_makt>.
                      IF  sy-subrc EQ 0 .
                        les_cab_ref-nom_cliente = <lfs_makt>-maktx.  "Texto breve de material
                      ENDIF.
                    ENDIF.
                    APPEND les_cab_ref TO c_det_cabecera.
                    CLEAR: les_cab_ref.
                  ENDLOOP.
*                  ENDIF.
                ELSE.
*            les_msg-num_doc = i_documento_origen.
                  les_msg-msg = 'No Existen Ordenes Pendientes por Picking con los Rangos de Fechas Ingresados' .
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                  RETURN.
                ENDIF.
              ELSE.
                CONCATENATE 'No Existen Componentes y/o Reservas para las Ordenes de Servicio de el aglutinador del Usuario'  space
                detalles_usuario-usuario space INTO l_e_msg RESPECTING BLANKS.
                les_msg-msg = l_e_msg.
                les_msg-type_msg = 'E'.
                APPEND les_msg TO c_mensajes.
                RETURN.
              ENDIF.
            ENDIF.
          ELSE.
            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.

*.....Si el Parametro Recibido por la Función es el Número de Orden de Servicio Interna
        ELSEIF i_documento_origen IS NOT INITIAL.
*.....Busco Información para la Clase de Documento
*          READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
*          IF sy-subrc EQ 0.
*.....Ordeno Tabla de Ordenes
          SORT lti_aufk BY aufnr ASCENDING.
*.....Consulto las Reservas para las Ordenes
          SELECT rsnum wempf kunnr aufnr
              FROM rkpf
                INTO TABLE lti_rkpf
                  FOR ALL ENTRIES IN lti_aufk
                      WHERE aufnr EQ lti_aufk-aufnr.

          IF sy-subrc EQ 0 .
*{   INSERT         ER3K900279                                       18
             IF lti_rkpf is not INITIAL.
*  .... Consulto las reservas para obtener la unidad de medida base y la cantidad
                SELECT *
                  INTO CORRESPONDING FIELDS OF TABLE lti_resb
                  FROM resb
                  FOR ALL ENTRIES IN lti_rkpf
                  WHERE rsnum EQ lti_rkpf-RSNUM AND
                  lgort eq detalles_usuario-almacen AND
                  bwart NE '262'.

                  SORT lti_resb by rsnum RSPOS.

                  LOOP AT lti_resb into les_resb.

                    IF les_resb-matnr is NOT INITIAL.
                        les_mara-matnr = les_resb-matnr.
                        APPEND les_mara TO lti_mara.
                        APPEND les_resb to lti_resb_x.
                    ENDIF.

                    AT END OF rsnum.
                      READ TABLE lti_resb into les_resb_x WITH KEY rsnum = les_resb-rsnum  XWAOK = 'X' .
                      IF sy-subrc ne 0.
                        READ TABLE lti_rkpf ASSIGNING <lfs_rkpf> WITH KEY rsnum = les_resb-rsnum.
                        IF sy-subrc eq 0.
                          DELETE lti_aufk WHERE aufnr = <lfs_rkpf>-aufnr.
                          DELETE lti_resb_x WHERE rsnum = les_resb-rsnum.
                        ENDIF.
                      ENDIF.
                    ENDAT.

                  ENDLOOP.
*                  Validamos que la tabla no este vacía por el FOR ALL ENTRIES
                 IF les_resb_x is not INITIAL.
                      "Seleccionamos los lotes para el material
                    SELECT matnr werks lgort charg clabs
                      INTO TABLE c_lotes
                      FROM mchb
                      FOR ALL ENTRIES IN lti_resb_x
                      WHERE matnr = lti_resb_x-matnr AND
                      werks = lti_resb_x-werks AND
                      lgort = lti_resb_x-lgort AND
                      lvorm NE 'X'.

                          "consultamos el objnr para luego obtener el status del equipo
                      SELECT MATNR b_werk b_lager SERNR equi~equnr EQTYP equi~objnr
                        INTO TABLE lti_equi
                        FROM equi
                        INNER JOIN EQBS on
                        equi~equnr = EQBS~equnr
                        FOR ALL ENTRIES IN lti_resb_x
                        WHERE matnr = lti_resb_x-matnr AND
                        b_werk = lti_resb_x-werks AND
                        b_LAGER = lti_resb_x-lgort AND
                        lvorm NE 'X'.


                        IF lti_equi is not INITIAL.

*                        Consulamos los estados para los objnr sean ALMA y ASAE
                          SELECt OBJNR STAT INACT
                            FROM JEST
                            into table lti_jest
                            FOR ALL ENTRIES IN lti_equi
                            WHERE objnr = lti_equi-objnr
                            and INACT = ' '
                            and ( STAT eq 'I0184' or STAT eq 'I0186' ).

*                          LOOP AT lti_equi into les_equi.
*                            READ TABLE lti_jest into  les_jest WITH KEY objnr = les_equi-objnr.
*                            IF sy-subrc eq 0.
*                                move les_equi to les_serno.
*                                APPEND  les_serno to C_SERIALES.
*                            ENDIF.
*
*                          ENDLOOP.

                        ENDIF.

                 ENDIF.

             ENDIF.
*}   INSERT
*{   DELETE         ER3K900279                                       19
*\            LOOP AT lti_rkpf ASSIGNING <lfs_rkpf>.
*\              l_e_cont = sy-tabix.
*\              CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
*\                EXPORTING
*\                  reservation_no    = <lfs_rkpf>-rsnum
*\                  movement          = 'X'
*\                TABLES
*\                  reservation_items = lti_res_items
*\                  return            = lti_bapiret2.
*\
*\*.....Lectura en Tabla de Retorno, los valores de los Campos de Key Identifican que no Esta Pend. por Picking
*\              READ TABLE lti_bapiret2 WITH KEY type = 'E'
*\                                               id = 'W5'
*\                                               number = 027 TRANSPORTING NO FIELDS.
*\*.....Valido que No Existan Reservas para la Orden
*\              IF sy-subrc EQ 0 .
*\                DELETE lti_aufk WHERE aufnr = <lfs_rkpf>-aufnr.
*\              ELSE.
*\*.....Ordeno Tabla para Identficar Cuales Posiciones Estan Pendientes por Picking
*\                SORT lti_res_items BY movement DESCENDING.
*\*.....Recorro Tablas de Reserva para Identificar si existe almenos 1 Posicion pendiente por Picking
*\                LOOP AT lti_res_items ASSIGNING <lfs_res_items>.
*\                  IF <lfs_res_items>-movement EQ 'X'.
*\                    EXIT.
*\                  ELSE.
*\*                    l_e_val = l_e_val + 1.
*\                  ENDIF.
*\                ENDLOOP.
*\              ENDIF.
*\              IF l_e_val IS NOT INITIAL.
*\*.....La tabla de Posiciones de Reseverva No Tiene Posiciones Pendientes por Picking
*\                DELETE lti_aufk WHERE aufnr = <lfs_rkpf>-aufnr.
*\              ENDIF.
*\              CLEAR : lti_res_items, lti_bapiret2, l_e_val.
*\            ENDLOOP.
*}   DELETE
            IF lti_aufk[] IS NOT INITIAL.
*{   INSERT         ER3K900279                                       14
*.....Función para Obtener EANs de un Material
                CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                TABLES
                    t_detalle  = c_eans
                    t_mara = lti_mara.

                IF lti_resb_x is not INITIAL.
*   Extraemos las ubicaciones, cantidades y unidades de medida del material
*   y lo llevamos a la tabla que se exportará
                    SELECT material ubicacion UBIC_DEFAULT cantidad unidad_med CENTRO ALMACEN
                    INTO CORRESPONDING FIELDS OF TABLE lti_ubicacion
                    FROM zmm_t_ubicacion
                    FOR ALL ENTRIES IN lti_resb_x
                    WHERE centro   EQ lti_resb_x-werks
                      AND almacen  EQ lti_resb_x-lgort
                      AND material EQ lti_resb_x-matnr.

*                    SELECT


                ENDIF.

*                IF lti_rkpf is not INITIAL.
**  .... Consulto las reservas para obtener la unidad de medida base y la cantidad
*                SELECT *
*                  INTO CORRESPONDING FIELDS OF TABLE lti_resb
*                  FROM resb
*                  FOR ALL ENTRIES IN lti_rkpf
*                  WHERE rsnum EQ lti_rkpf-RSNUM AND
*                  lgort eq detalles_usuario-almacen AND
*                  bwart NE '262'.
*                ENDIF.

*                lti_res_items = lti_res_items_x.
*}   INSERT


*.....Recorro Tabla de Ordenes
              LOOP AT lti_aufk ASSIGNING <lfs_aufk>.
*{   INSERT         ER3K900279                                       10
                CLEAR: les_cab_ref.
*}   INSERT
                UNASSIGN <lfs_zppt_001>.
                READ TABLE lti_zppt_0001 ASSIGNING <lfs_zppt_001> WITH KEY nm_ref_h = <lfs_aufk>-aufnr.
*                IF <lfs_zppt_001> IS ASSIGNED.
*                  les_cab_ref-aufnr =  <lfs_zppt_001>-nm_ref_p. " numero de orden proceso pieza
*                ENDIF.
                IF <lfs_zppt_001> IS ASSIGNED.
                  les_cab_ref-aufnr =  <lfs_zppt_001>-nm_ref_h. " numero de orden proceso pieza
                ENDIF.
                les_cab_ref-aglutinador = l_e_nmint. " aglutinador
                les_cab_ref-aglutinador = l_e_nmint.
                les_cab_ref-num_doc     = <lfs_aufk>-aufnr.   "Número de Orden
                les_cab_ref-fecha_crea  = <lfs_aufk>-erdat.   "Fecha Entrada o Creación de la Orden
                les_cab_ref-tipo_doc    = <lfs_aufk>-auart.   "Clase de Orden
*.....Lectura a Tabla de Orden para el Material
                READ TABLE lti_afko WITH KEY aufnr = <lfs_aufk>-aufnr ASSIGNING <lfs_afko>.
                IF sy-subrc EQ 0.
                  les_cab_ref-cod_cliente = <lfs_afko>-plnbez.   "Número de material
*.....Lectura a Tabla de Texto de Material
                  READ TABLE lti_makt WITH  KEY matnr = <lfs_afko>-plnbez ASSIGNING <lfs_makt>.
                  IF  sy-subrc EQ 0 .
                    les_cab_ref-nom_cliente = <lfs_makt>-maktx.  "Texto breve de material
                  ENDIF.
                ENDIF.

                APPEND les_cab_ref TO c_det_cabecera.
*{   DELETE         ER3K900279                                        9
*\                CLEAR: les_cab_ref.
*}   DELETE
*{   INSERT         ER3K900279                                       11
                IF I_FLAG_DET EQ 'X'.
*.....Obtenemos la reserva de la orden
                READ TABLE lti_rkpf ASSIGNING <LFS_RKPF> WITH KEY aufnr = <lfs_aufk>-aufnr.
                IF sy-subrc eq 0.
*                lti_res_items_x = lti_res_items.
*                DELETE lti_res_items_x where RESERV_NO NE <LFS_RKPF>-RSNUM.
*
                lti_resb = lti_resb_x.
                DELETE lti_resb where rsnum NE <LFS_RKPF>-RSNUM.

                ENDIF.
*                LOOP AT lti_res_items_x ASSIGNING <lfs_res_items> WHERE movement = 'X' and stge_loc eq detalles_usuario-almacen and RESERV_NO eq <LFS_RKPF>-RSNUM.
                LOOP AT lti_resb into les_resb WHERE XWAOK = 'X' and lgort eq detalles_usuario-almacen and rsnum eq <LFS_RKPF>-RSNUM.
*... Consulto el registro en la reserva
*                READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_res_items>-reserv_no
*                                         rspos = <lfs_res_items>-res_item.
*                IF sy-subrc ne 0.
*                  continue.
*                ELSE.
*                  filtro de cantidad tomada
                  IF les_resb-ENMNG = 0 and les_resb-KZEAR = 'X'.

*                  IF les_resb-KZEAR = 'X'.
                      continue.
                  ELSEIF les_resb-ENMNG = 0 and les_resb-XLOEK = 'X'.

                      continue.
                  ELSEIF les_resb-ENMNG > 0 and les_resb-KZEAR = 'X'.
                    les_resb-bdmng =  les_resb-ENMNG.
                    les_resb-erfmg =  les_resb-ENMNG.
                    les_resb-erfme =  les_resb-meins.

                  ENDIF.
*                ENDIF.
*.....Inserto Registro con Número de Material
*                l_e_matnr = <lfs_res_items>-material.
*                 l_e_matnr =  les_resb-matnr.
*                APPEND l_e_matnr TO lti_matnr.
*.....Limpieza de Variables
                CLEAR : lti_return, lti_serno, lti_charg, lti_estatin.
**.....Función para Obtener el la Serie y el Número de Lote para un Material
*                CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
*                  EXPORTING
*                    werk      = les_resb-werks"<lfs_res_items>-plant       "Centro
*                    lgort     = les_resb-lgort"<lfs_res_items>-stge_loc    "Almacen
*                    username  = detalles_usuario-usuario
*                  TABLES
*                    t_return  = lti_return
*                    t_serno   = lti_serno     "Serie para el Material
*                    t_charg   = lti_charg     "Lote para el Material
*                    t_estatin = lti_estatin
*                    t_matnr   = lti_matnr.

**.....Verifico que no Existan Errores para el Material
*                READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
*                IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.

*.....Limpieza de Variables
                  CLEAR : lti_detalle, lti_mensajes, lti_matnr,les_det_pos,les_equi.
*.....Verifico que el Material Tenga Número de Serie
*                  READ  TABLE lti_equi into les_equi WITH KEY matnr = les_resb-matnr werks = les_resb-werks lgort = les_resb-lgort.
*                  IF sy-subrc eq 0.
*                    les_det_pos-seriales = 'X'.
*                  ELSE.
*                    les_det_pos-seriales = ' '.
*                  ENDIF.
                      les_det_pos-seriales = ' '.
                      LOOP AT lti_equi into les_equi where matnr = les_resb-matnr and werks = les_resb-werks and lgort = les_resb-lgort.
                            READ TABLE lti_jest into  les_jest WITH KEY objnr = les_equi-objnr.
                            IF sy-subrc eq 0.
                                les_det_pos-seriales = 'X'.
                                move les_equi to les_serno.
                                APPEND  les_serno to C_SERIALES.
                            ENDIF.

                      ENDLOOP.

*                  IF lti_serno IS NOT INITIAL.
**                    APPEND LINES OF lti_serno TO c_seriales.
*                    les_det_pos-seriales = 'X'.
*                  ELSE.
*                    les_det_pos-seriales = ' '.
*                  ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                  READ TABLE c_lotes ASSIGNING <lfs_charg> WITH KEY MATNR = les_resb-matnr WERKS = les_resb-werks LGORT = les_resb-lgort.
                  IF sy-subrc eq 0.
*                    APPEND LINES OF lti_charg TO c_lotes.
                    les_det_pos-numero_lote = 'X'.
                  ELSE.
                    les_det_pos-numero_lote = ' '.
                  ENDIF.

*{   REPLACE        ER3K900279                                        5
*\*.....Función para Obtener EANs de un Material
*\                  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\                    EXPORTING
*\                      matnr      = <lfs_res_items>-material
*\                    TABLES
*\                      t_detalle  = lti_detalle
*\                      t_mensajes = lti_mensajes.
*\*.....Consulto Tabla de Mensajes para Verificar que no Existan Errores en los EAN´s del Material
*\                  READ TABLE lti_mensajes  INDEX 1 ASSIGNING  <lfs_mensajes>.
*\                  IF sy-subrc EQ 0 .
*\                    les_msg-num_doc = <lfs_mensajes>-num_doc.
*\                    les_msg-msg = <lfs_mensajes>-msg.
*\                    les_msg-type_msg = <lfs_mensajes>-msg.
*\                    APPEND les_msg TO c_mensajes.
*\                  ELSE.
*\*************************************
*\**... Consulto el registro en la reserva
*\*                    READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_res_items>-reserv_no
*\*                                         rspos = <lfs_res_items>-res_item.
*\*... Convierto la cantidad total de la reserva y la convierto a UMB
*\                    READ TABLE lti_detalle assigning <lfs_detalle> WITH KEY unidad_medida = les_resb-erfme.
*\                    IF sy-subrc EQ 0.
*\*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
*\                      IF les_resb IS NOT INITIAL.
*\
*\                        <lfs_res_items>-entry_qnt = les_resb-erfmg  *  <lfs_detalle>-cantidad_ean.
*\                        <lfs_res_items>-entry_uom = les_resb-meins.
*\                      ENDIF.
*\                    ELSE.
*\*....Asigno la unidad de medida base y la cantidad si la encuentra
*\                      IF les_resb IS NOT INITIAL.
*\                        <lfs_res_items>-entry_qnt = les_resb-bdmng.
*\                        <lfs_res_items>-entry_uom = les_resb-meins.
*\                      ENDIF.
*\                    ENDIF.
*\**********************************************
*\*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
*\                    IF lti_detalle IS NOT INITIAL.
*\                      LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
*\                        les_det_pos-num_entrega = <lfs_rkpf>-aufnr.
*\                        les_det_pos-pos_entrega = <lfs_res_items>-res_item.
*\                        les_det_pos-material = <lfs_res_items>-material.
*\                        les_det_pos-desc_material = <lfs_res_items>-item_text.
*\                        les_det_pos-cant_por_pick = <lfs_res_items>-entry_qnt.
*\                        les_det_pos-ubic_fija = ' '.
*\                        les_det_pos-ubic_tmp =  ' '.
*\                        les_det_pos-ubic_sugerida = ' '.
*\                        les_det_pos-cant_contada =  ' '.
*\*.....Realizar la conversión de la unidad de medida
*\                        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*\                          EXPORTING
*\                            input          = <lfs_res_items>-entry_uom
*\                            language       = 'S'
*\                          IMPORTING
*\                            output         = l_e_meinh
*\                          EXCEPTIONS
*\                            unit_not_found = 1
*\                            OTHERS         = 2.
*\                        IF l_e_meinh EQ '004'.
*\                          les_det_pos-uni_med_doc = <lfs_res_items>-entry_uom.
*\                        ELSE.
*\                          les_det_pos-uni_med_doc = l_e_meinh.
*\                        ENDIF.
*\                        les_det_pos-seriales = <lfs_res_items>-seriales.
*\                        les_det_pos-numero_lote =  <lfs_res_items>-numero_lote.
*\*.....Datos de Detalle EAN
*\                        les_det_pos-ean = <lfs_detalle>-ean.
*\                        les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*\                        les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
*\                        APPEND les_det_pos TO c_det_posiciones.
*\                        CLEAR les_det_pos.
*\                      ENDLOOP.
*\                    ELSE.
*\                      les_msg-num_doc = ' '.
*\                      CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_res_items>-material
*\                      INTO l_e_msg RESPECTING BLANKS.
*\                      les_msg-msg = l_e_msg.
*\                      les_msg-type_msg = 'E'.
*\                      APPEND les_msg TO c_mensajes.
*\                    ENDIF.
*\                  ENDIF.
                  IF I_FLAG_AGRUP IS NOT INITIAL.
*... Convierto la cantidad total de la reserva y la convierto a UMB
                    READ TABLE c_eans ASSIGNING   <lfs_detalle> WITH KEY matnr = les_resb-matnr unidad_medida = les_resb-erfme.
                    IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.

                        les_det_pos-cant_por_pick = les_resb-erfmg  *  <lfs_detalle>-cantidad_ean.
                        les_det_pos-uni_med_doc = les_resb-meins.
                      ENDIF.
                    ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.
                        les_det_pos-cant_por_pick = les_resb-bdmng.
                        les_det_pos-uni_med_doc = les_resb-meins.
                      ENDIF.
                    ENDIF.
**********************************************
                        les_det_pos-num_entrega = <lfs_rkpf>-aufnr.
*                        les_det_pos-pos_entrega = <lfs_res_items>-res_item.
                        les_det_pos-material = les_resb-matnr.
                        READ TABLE c_eans ASSIGNING <lfs_detalle> WITH KEY matnr = les_resb-matnr.
                        IF sy-subrc eq 0.
                          les_det_pos-desc_material = <lfs_detalle>-MAKTX.
                        ENDIF.

*                        les_det_pos-cant_por_pick = <lfs_res_items>-entry_qnt.
                        les_det_pos-ubic_fija = ' '.
                        les_det_pos-ubic_tmp =  ' '.
                        READ TABLE lti_zppt_0001 into les_zppt_0001 with key NM_REF_H = <lfs_rkpf>-aufnr.
                        IF sy-subrc eq 0.
*.... Aglutinador
                          les_det_pos-aglutinador = les_zppt_0001-nmint.
*.... Proceso pieza l_e_NM_REF_P
                        les_det_pos-NM_REF_P = les_zppt_0001-NM_REF_h.
                        ENDIF.
                        READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X' CENTRO = les_resb-werks
                        ALMACEN = les_resb-lgort.
                        IF sy-subrc eq 0.
                          les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                          ELSE.
                            READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material CENTRO = les_resb-werks
                        ALMACEN = les_resb-lgort.
                            IF sy-subrc eq 0.
                              les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                            ENDIF.
                        ENDIF.

                        les_det_pos-cant_contada =  ' '.
*.....Realizar la conversión de la unidad de medida
                        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                          EXPORTING
                            input          = les_det_pos-uni_med_doc
                            language       = 'S'
                          IMPORTING
                            output         = l_e_meinh
                          EXCEPTIONS
                            unit_not_found = 1
                            OTHERS         = 2.
                        IF l_e_meinh NE '004'.
                           les_det_pos-uni_med_doc = l_e_meinh.
                        ENDIF.

*                        les_det_pos-seriales = <lfs_res_items>-seriales.
*                        les_det_pos-numero_lote =  <lfs_res_items>-numero_lote.

*.....Datos de Detalle EAN
*                        les_det_pos-ean = <lfs_detalle>-ean.
*                        les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
*                        les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                         READ TABLE c_eans ASSIGNING  <lfs_detalle> WITH KEY matnr = les_det_pos-material UNIDAD_MEDIDA = les_det_pos-uni_med_doc.
                      IF sy-subrc eq 0.
                      les_det_pos-ean = <lfs_detalle>-ean.
                      les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                      les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                      ENDIF.

                      COLLECT les_det_pos INTO c_det_posiciones.
                      MODIFY c_det_posiciones from les_det_pos index sy-tabix TRANSPORTING cantidad_ean.




                  else.
*    .....Función para Obtener EANs de un Material
                      CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                        EXPORTING
                          matnr      = <lfs_res_items>-material
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
*    ************************************
*    *... Consulto el registro en la reserva
*                        READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_res_items>-reserv_no
*                                             rspos = <lfs_res_items>-res_item.
*... Convierto la cantidad total de la reserva y la convierto a UMB
                    READ TABLE c_eans ASSIGNING   <lfs_detalle> WITH KEY matnr = les_resb-matnr unidad_medida = les_resb-erfme.
                    IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.

                        les_det_pos-cant_por_pick = les_resb-erfmg  *  <lfs_detalle>-cantidad_ean.
                        les_det_pos-uni_med_doc = les_resb-meins.
                      ENDIF.
                    ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.
                        les_det_pos-cant_por_pick = les_resb-bdmng.
                        les_det_pos-uni_med_doc = les_resb-meins.
                      ENDIF.
                    ENDIF.

*    *********************************************
*    .....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
                        IF lti_detalle IS NOT INITIAL.
                          LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                            les_det_pos-num_entrega = <lfs_rkpf>-aufnr.
                            les_det_pos-pos_entrega = les_resb-rspos.
                            les_det_pos-material = les_resb-matnr.
*                            les_det_pos-desc_material = <lfs_res_items>-item_text.
                             READ TABLE c_eans ASSIGNING <lfs_detalle> WITH KEY matnr = les_resb-matnr.
                        IF sy-subrc eq 0.
                          les_det_pos-desc_material = <lfs_detalle>-MAKTX.
                        ENDIF.

*                            les_det_pos-cant_por_pick = <lfs_res_items>-entry_qnt.
                            les_det_pos-ubic_fija = ' '.
                            les_det_pos-ubic_tmp =  ' '.
                            READ TABLE lti_zppt_0001 into les_zppt_0001 with key NM_REF_H = <lfs_rkpf>-aufnr.
                            IF sy-subrc eq 0.
*    .... Aglutinador
                              les_det_pos-aglutinador = les_zppt_0001-nmint.

*    .... Proceso pieza l_e_NM_REF_P
                            les_det_pos-NM_REF_P = les_zppt_0001-NM_REF_h.

                            ENDIF.
                          READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material UBIC_DEFAULT = 'X' CENTRO = les_resb-werks
                          ALMACEN = les_resb-lgort.
                          IF sy-subrc eq 0.
                            les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                            ELSE.
                              READ TABLE lti_ubicacion into les_ubicacion WITH KEY material = les_det_pos-material CENTRO = les_resb-werks
                          ALMACEN = les_resb-lgort.
                              IF sy-subrc eq 0.
                                les_det_pos-UBIC_SUGERIDA = les_ubicacion-UBICACION.
                              ENDIF.
                          ENDIF.

                            les_det_pos-cant_contada =  ' '.
*    .....Realizar la conversión de la unidad de medida
                            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                              EXPORTING
*                                input          = <lfs_res_items>-entry_uom
                                 input          = les_det_pos-uni_med_doc
                                language       = 'S'
                              IMPORTING
                                output         = l_e_meinh
                              EXCEPTIONS
                                unit_not_found = 1
                                OTHERS         = 2.
                            IF l_e_meinh NE '004'.
                               les_det_pos-uni_med_doc = l_e_meinh.
                            ENDIF.
*                            les_det_pos-seriales = <lfs_res_items>-seriales.
*                            les_det_pos-numero_lote =  <lfs_res_items>-numero_lote.
*    .....Datos de Detalle EAN
                            les_det_pos-ean = <lfs_detalle>-ean.
                            les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                            les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                            APPEND les_det_pos TO c_det_posiciones.
                            CLEAR les_det_pos.
                          ENDLOOP.
                        ELSE.
                          les_msg-num_doc = ' '.
                          CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_res_items>-material
                          INTO l_e_msg RESPECTING BLANKS.
                          les_msg-msg = l_e_msg.
                          les_msg-type_msg = 'E'.
                          APPEND les_msg TO c_mensajes.
                        ENDIF.
                      ENDIF.
                  endIF.
*}   REPLACE
*{   DELETE         ER3K900339                                       20
*\                ELSE.
*\                  les_msg-num_doc = <lfs_rkpf>-aufnr.
*\                  les_msg-msg = <lfs_return>-message.
*\                  les_msg-type_msg = 'E'.
*\                  APPEND les_msg TO c_mensajes.
*\                ENDIF.
*}   DELETE
              ENDLOOP.
                ENDIF.
*}   INSERT
              ENDLOOP.
*              ENDIF.
            ELSE.
              les_msg-num_doc = i_documento_origen.
              les_msg-msg = 'La Orden Servicio Interna No esta Pendiente por Picking ' .
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
              RETURN.
            ENDIF.
          ELSE.
            CONCATENATE 'No Existen Componentes y/o Reservas para la Orden de Servicio'  space
            detalles_usuario-usuario space INTO l_e_msg RESPECTING BLANKS.
            les_msg-num_doc = i_documento_origen.
            les_msg-msg = l_e_msg.
            les_msg-type_msg = 'E'.
            APPEND les_msg TO c_mensajes.
            RETURN.
          ENDIF.
*          ELSE.
*            les_msg-num_doc = i_documento_origen.
*            les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
*            les_msg-type_msg = 'E'.
*            APPEND les_msg TO c_mensajes.
*            RETURN.
*          ENDIF.
        ELSE.
*            les_msg-num_doc = i_documento_origen.
          les_msg-msg = 'Debe Ingresar Rango de Fechas o Número de Orden'.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
      ENDIF.
    ELSE.
      c_mensajes[] = mensajes[].
      RETURN.
    ENDIF.
  ENDIF.

  SORT c_det_cabecera BY aglutinador aufnr num_doc.
  DELETE ADJACENT DUPLICATES FROM c_det_cabecera COMPARING aglutinador aufnr.

ENDMETHOD.


METHOD ZIF_LGTICA_DOCUMENTO~GET_DETALLE.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Detalle para Ordenes de Prod. Sin Aglutinador
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 14.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 14.08.2014     ER6K907163    Marco Suarez        Creación
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

*.....Typo para Datos Completos de Reservas
  TYPES: BEGIN OF ltp_reservas,
      reserv_no TYPE  rsnum,
      res_item TYPE	rspos,
      material TYPE	matnr,
      plant TYPE  werks_d,
      stge_loc TYPE	lgort_d,
      batch TYPE  charg_d,
      entry_qnt TYPE erfmg,
      entry_uom TYPE erfme,
      entry_uom_iso TYPE isocd_unit,
      req_date TYPE	bdter,
      gl_account TYPE	saknr,
      item_text	 TYPE sgtxt,
      gr_rcpt TYPE  wempf,
      unload_pt	 TYPE ablad,
      fixed_quan TYPE	fmeng,
      movement TYPE	xwaok,
      withd_quan TYPE	enmng,
      base_uom TYPE	meins,
      base_uom_iso TYPE	isocd_unit,
      process_id TYPE	beakz,
      material_external TYPE  mgv_material_external,
      material_guid TYPE  mgv_material_guid,
      material_version TYPE	mgv_material_version,
*{   INSERT         ER3K900279                                        1
     STK_SEGMENT  Type  SGT_SCAT,
     REQ_SEGMENT  Type  SGT_RCAT,
*}   INSERT
      seriales TYPE  c LENGTH 1 ,  "ZMM_FM_CNSLTA_SERLOT = X Si tiene Serial
      numero_lote TYPE c LENGTH 1, "ZMM_FM_CNSLTA_SERLOT = X Si tiene Número de Lote
 END OF ltp_reservas.

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

*.....Typo para Datos de Reservas de Ordenes
  TYPES: BEGIN OF ltp_rkpf,
         rsnum TYPE rsnum,    "Número de la reserva/las necesidades secundarias
         wempf TYPE wempf,    "Destinatario de mercancías
         kunnr TYPE ekunn,    "Número de cuenta del cliente
         aufnr TYPE aufnr,    "Número de orden
        END OF ltp_rkpf.

  DATA :
*..... Tabla interna con datos de la tabla de el aglutinador y la asociación de las ordenes
        lti_zppt_0001 TYPE TABLE OF ltp_zppt_0001,
        les_zppt_0001 TYPE ltp_zppt_0001,
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
*.....Variable con el Número de Material
           l_e_matnr TYPE matnr,
*.....Tabla Internas de Retorno al Consultar la Actividad
           lti_actividad TYPE TABLE OF zmm_t_clase_pv,
*.....Variable para Unidad de medida despues de la Conversion
           l_e_meinh TYPE string,
*.....Tabla Interna con datos Basicos de Orden
           lti_aufk TYPE TABLE OF ltp_aufk,
*.....Tabla Interna con datos Basico de Reservas
           lti_rkpf TYPE TABLE OF ltp_rkpf,
*.... Estructuras para buscar las reservas en la tabla resb
  lti_resb TYPE STANDARD TABLE OF resb,
        les_resb TYPE resb,
*.... Aglutinador
           l_e_nmint TYPE zppde_0002,
*..... Proceso pieza
           l_e_NM_REF_h TYPE ZPPDE_0006,
*.....Mensaje
           l_e_msg TYPE string,
*.....Variables para Bapi
           lti_bapiret2 TYPE bapiret2_tab,
           lti_res_items TYPE TABLE OF ltp_reservas. " bapi2093_res_items_get.

  DATA: lti_mara  TYPE TABLE OF mara,
        les_mara TYPE mara.

  FIELD-SYMBOLS : <lfs_return> TYPE bapiret1,
                  <lfs_serno> TYPE zstmm_0003,
                  <lfs_charg> TYPE zstmm_0002,
                  <lfs_detalle> TYPE zesd_eanmat,
                  <lfs_mensajes> TYPE zesd_msg_picking,
                  <lfs_actividad> TYPE zmm_t_clase_pv,
                  <lfs_aufk> TYPE ltp_aufk,
                  <lfs_rkpf> TYPE ltp_rkpf,
                  <lfs_res_items> TYPE  ltp_reservas. "bapi2093_res_items_get.

*.....Función para Obtener Información de los Procesos de Picking
  CALL FUNCTION 'ZMM_FM_CONSULTA_TIPO_ACTIVIDAD'
    EXPORTING
      proceso         = 'PICKING'
      cod_modulo      = i_tipo_documento        "OSIN
    TABLES
      tipos_actividad = lti_actividad
      t_mensajes      = c_mensajes.

  IF c_mensajes IS NOT INITIAL  .
    RETURN.
  ELSE.
*.....Valido si Existe Información para el Usuario del Dispositivo
    IF detalles_usuario IS NOT INITIAL.
*.....Ajusto Número de Documento
      MOVE i_documento_origen TO l_e_nmint.
      MOVE i_documento_origen TO l_e_NM_REF_h.

*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_e_nmint
          IMPORTING
            output = l_e_nmint.
*.....Función para Ajuste de Número
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_e_NM_REF_h
          IMPORTING
            output = l_e_NM_REF_h
            .
*.... Busqueda de aglutinador en la tabla   Agrupador Ordenes de producción
        SELECT * FROM ZPPT_0001
          INTO CORRESPONDING FIELDS OF TABLE lti_zppt_0001
          WHERE nmint EQ l_e_nmint OR
          NM_REF_h EQ l_e_NM_REF_h
          order by NM_REF_h ASCENDING.

*.....Busqueda de Ordenes de Servicio Internas
      SELECT aufnr auart autyp ernam erdat werks user0 kdauf
                 FROM aufk
                  INTO TABLE lti_aufk
                    for all entries in lti_zppt_0001
                    WHERE werks EQ detalles_usuario-centro AND
                          aufnr EQ lti_zppt_0001-NM_REF_H.

*.....Verifico que Existan Registros
      IF lti_aufk[] IS NOT INITIAL.
*.....Ordeno la Tabla para el For All Entries
        SORT lti_aufk BY aufnr ASCENDING.
*.....Busco Información para la Clase de Documento
*        READ TABLE lti_actividad WITH KEY cod_clase = i_clase_documento ASSIGNING <lfs_actividad>.
*        IF sy-subrc EQ 0.
*.....Consulto las Reservas para las Ordenes
        SELECT rsnum wempf kunnr aufnr
            FROM rkpf
              INTO TABLE lti_rkpf
                FOR ALL ENTRIES IN lti_aufk
                    WHERE aufnr EQ lti_aufk-aufnr
                    .

        IF sy-subrc EQ 0.
          LOOP AT lti_rkpf ASSIGNING <lfs_rkpf>.
            CALL FUNCTION 'BAPI_RESERVATION_GETITEMS1'
              EXPORTING
                reservation_no    = <lfs_rkpf>-rsnum
                WITHDRAWN          = 'X'
                movement          = 'X'
              TABLES
                reservation_items = lti_res_items
                return            = lti_bapiret2.

            IF lti_res_items[] IS NOT INITIAL.

*-----------------------------------------------------------------------
*       .....MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño
*            Modificado por: Andrés Felipe Castro.
               LOOP AT lti_res_items ASSIGNING <lfs_res_items>.
                 les_mara-matnr = <lfs_res_items>-material.
                 APPEND les_mara TO lti_mara.
               ENDLOOP.

*       .....Función para Obtener EANs de un Material
               CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                 TABLES
                   t_mara = lti_mara.
*       .....FIN_MODIFICACIÓN: 02/07/2015 - Ajustes de desdempeño.
*------------------------------------------------------------------------
*.... Consulto las reservas para obtener la unidad de medida base y la cantidad
              SELECT *
                INTO CORRESPONDING FIELDS OF TABLE lti_resb
                FROM resb
                FOR ALL ENTRIES IN lti_res_items
                WHERE rsnum EQ lti_res_items-reserv_no AND
                rspos EQ lti_res_items-res_item AND bwart NE '262' and
                lgort eq detalles_usuario-almacen.

*.....Solo Recorro las Posiciones Pendientes por Picking
              LOOP AT lti_res_items ASSIGNING <lfs_res_items> WHERE movement = 'X' and stge_loc eq detalles_usuario-almacen.
*... Consulto el registro en la reserva
                READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_res_items>-reserv_no
                                         rspos = <lfs_res_items>-res_item.
                IF sy-subrc ne 0.
                  continue.
                ELSE.
*                  filtro de cantidad tomada
*                  IF les_resb-ENMNG = 0 and les_resb-KZEAR = 'X'.
                  IF les_resb-KZEAR = 'X'.
                      continue.
                  ENDIF.
                ENDIF.
*.....Inserto Registro con Número de Material
                l_e_matnr = <lfs_res_items>-material.
                APPEND l_e_matnr TO lti_matnr.
*.....Limpieza de Variables
                CLEAR : lti_return, lti_serno, lti_charg, lti_estatin.
*.....Función para Obtener el la Serie y el Número de Lote para un Material
                CALL FUNCTION 'ZMM_FM_CNSLTA_SERLOT'
                  EXPORTING
                    werk      = <lfs_res_items>-plant       "Centro
                    lgort     = <lfs_res_items>-stge_loc    "Almacen
                    username  = detalles_usuario-usuario
                  TABLES
                    t_return  = lti_return
                    t_serno   = lti_serno     "Serie para el Material
                    t_charg   = lti_charg     "Lote para el Material
                    t_estatin = lti_estatin
                    t_matnr   = lti_matnr.
*.....Verifico que no Existan Errores para el Material
                READ TABLE lti_return INDEX 1 ASSIGNING <lfs_return>.
                IF sy-subrc NE 0 OR <lfs_return>-type NE 'E'.
*.....Verifico que el Material Tenga Número de Serie
                  IF lti_serno IS NOT INITIAL.
                    APPEND LINES OF lti_serno TO c_seriales.
                    <lfs_res_items>-seriales = 'X'.
                  ELSE.
                    <lfs_res_items>-seriales = ' '.
                  ENDIF.
*.....Verifico si el Material Tiene Número de Lote
                  IF lti_charg  IS NOT INITIAL  .
                    APPEND LINES OF lti_charg TO c_lotes.
                    <lfs_res_items>-numero_lote = 'X'.
                  ELSE.
                    <lfs_res_items>-numero_lote = ' '.
                  ENDIF.
*.....Limpieza de Variables
                  CLEAR : lti_detalle, lti_mensajes, lti_matnr.
*.....Función para Obtener EANs de un Material
                  CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
                    EXPORTING
                      matnr      = <lfs_res_items>-material
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
*************************************
**... Consulto el registro en la reserva
*                    READ TABLE lti_resb INTO les_resb WITH KEY rsnum = <lfs_res_items>-reserv_no
*                                         rspos = <lfs_res_items>-res_item.
*... Convierto la cantidad total de la reserva y la convierto a UMB
                    READ TABLE lti_detalle assigning <lfs_detalle> WITH KEY unidad_medida = les_resb-erfme.
                    IF sy-subrc EQ 0.
*....Asigno el calculo de la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.

                        <lfs_res_items>-entry_qnt = les_resb-erfmg  *  <lfs_detalle>-cantidad_ean.
                        <lfs_res_items>-entry_uom = les_resb-meins.
                      ENDIF.
                    ELSE.
*....Asigno la unidad de medida base y la cantidad si la encuentra
                      IF les_resb IS NOT INITIAL.
                        <lfs_res_items>-entry_qnt = les_resb-bdmng.
                        <lfs_res_items>-entry_uom = les_resb-meins.
                      ENDIF.
                    ENDIF.
**********************************************

*.....Consulto Tabla de Detalle para Verificar EAN, Cantidad EAN y Unidad de Medida
                    IF lti_detalle IS NOT INITIAL.
                      LOOP AT lti_detalle ASSIGNING <lfs_detalle> .
                        les_det_pos-num_entrega = <lfs_rkpf>-aufnr.
                        les_det_pos-pos_entrega = <lfs_res_items>-res_item.
                        les_det_pos-material = <lfs_res_items>-material.
                        les_det_pos-desc_material = <lfs_res_items>-item_text.
                        les_det_pos-cant_por_pick = <lfs_res_items>-entry_qnt.
                        les_det_pos-ubic_fija = ' '.
                        les_det_pos-ubic_tmp =  ' '.
                        les_det_pos-ubic_sugerida = ' '.
                        les_det_pos-cant_contada =  ' '.

                        READ TABLE lti_zppt_0001 into les_zppt_0001 with key NM_REF_H = <lfs_rkpf>-aufnr.
                        IF sy-subrc eq 0.
*.... Aglutinador
                          les_det_pos-aglutinador = les_zppt_0001-nmint.

*.... Proceso pieza l_e_NM_REF_P
                        les_det_pos-NM_REF_P = les_zppt_0001-NM_REF_h.

                        ENDIF.
*.... Orden
*                        les_det_pos-aufnr = <lfs_res_items>-aufnr.
*.....Realizar la conversión de la unidad de medida
                        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                          EXPORTING
                            input          = <lfs_res_items>-entry_uom
                            language       = 'S'
                          IMPORTING
                            output         = l_e_meinh
                          EXCEPTIONS
                            unit_not_found = 1
                            OTHERS         = 2.
                        IF l_e_meinh EQ '004'.
                          les_det_pos-uni_med_doc = <lfs_res_items>-entry_uom.
                        ELSE.
                          les_det_pos-uni_med_doc = l_e_meinh.
                        ENDIF.
                        les_det_pos-seriales = <lfs_res_items>-seriales.
                        les_det_pos-numero_lote =  <lfs_res_items>-numero_lote.
*.....Datos de Detalle EAN
                        les_det_pos-ean = <lfs_detalle>-ean.
                        les_det_pos-cantidad_ean = <lfs_detalle>-cantidad_ean.
                        les_det_pos-uni_med_ean = <lfs_detalle>-unidad_medida.
                        APPEND les_det_pos TO c_det_posiciones.
                        CLEAR les_det_pos.
                      ENDLOOP.
                    ELSE.
                      les_msg-num_doc = ' '.
                      CONCATENATE 'No Existe EAN, Cantidad EAN ó Unidad de Medida para el ' <lfs_res_items>-material
                      INTO l_e_msg RESPECTING BLANKS.
                      les_msg-msg = l_e_msg.
                      les_msg-type_msg = 'E'.
                      APPEND les_msg TO c_mensajes.
                    ENDIF.
                  ENDIF.
                ELSE.
                  les_msg-num_doc = <lfs_rkpf>-aufnr.
                  les_msg-msg = <lfs_return>-message.
                  les_msg-type_msg = 'E'.
                  APPEND les_msg TO c_mensajes.
                ENDIF.
              ENDLOOP.
            ELSE.
              les_msg-num_doc = i_documento_origen.
              les_msg-msg = 'La Orden Servicio Interna No esta Pendiente por Picking ' .
              les_msg-type_msg = 'E'.
              APPEND les_msg TO c_mensajes.
*              RETURN.
            ENDIF.
          ENDLOOP.
        ELSE.
          CONCATENATE 'No Existen Componentes y/o Reservas para la Orden de Servicio'  space
          detalles_usuario-usuario space INTO l_e_msg RESPECTING BLANKS.
          les_msg-num_doc = i_documento_origen.
          les_msg-msg = l_e_msg.
          les_msg-type_msg = 'E'.
          APPEND les_msg TO c_mensajes.
          RETURN.
        ENDIF.
*        ELSE.
*          les_msg-num_doc = i_documento_origen.
*          les_msg-msg =  'La Clase de Documento no esta Correctamente Parametrizada'.
*          les_msg-type_msg = 'E'.
*          APPEND les_msg TO c_mensajes.
*          RETURN.
*        ENDIF.
      ELSE.
        les_msg-num_doc = i_documento_origen.
        les_msg-msg = 'Usuario No Autorizado para Centro y/o Almacen'.
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


METHOD ZIF_LGTICA_DOCUMENTO~LOAD_DOCUMENT.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Setea Atributos de los aglutinadores
* Autor Prog.  :
* Fecha Creac. : 14.08.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*.....Metodo para Setiar Atributos de Aglutinador
  CALL METHOD me->zif_lgtica_documento~SET_AGLUTINADOR
    EXPORTING
      i_clase_documento  = i_clase_documento
      i_documento_origen = i_documento_origen.

*.....Indicador para Proceso que Genera Entrega
  c_indicador = ' '.

ENDMETHOD.


METHOD zif_lgtica_pickable~get_header_picking.
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
* Ordenes
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de aglutinadores
  DATA: les_ordenes_aglut TYPE zesd_cabecera_aglut.
*.... Estructura para el manejo del picking
  DATA: les_header_picking TYPE zedsd_picking.

*.... Leo la cabecera de aglutinador
  IF cabecera_aglutinador IS NOT INITIAL.
    LOOP AT cabecera_aglutinador INTO les_ordenes_aglut.
      les_header_picking-mandt = sy-mandt.
*les_header_picking-PICKNUM =
*les_header_picking-PROCESO =
*les_header_picking-SUBPROCESO =

*      les_header_picking-rsnum = les_ordenes_aglut-rsnum.
      les_header_picking-aufnr = les_ordenes_aglut-NM_REF_h.
*    les_header_picking-ebeln = les_ventas-ebeln.
      les_header_picking-AGLUTINADOR = les_ordenes_aglut-aglutinador.
      les_header_picking-rsnum  = les_ordenes_aglut-rsnum.
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
* ORDENES
*-------------------------------------------------------------------------------*
*.... Estructura para el manejo de detalle de reservas
  data: les_reservas_dt type BAPI2093_RES_ITEMS_GET.

*.... Estructura para el manejo de la cabecera de reservas
 data: les_cabe_aglut type ZESD_CABECERA_AGLUT.

*.... Estructura para el manejo del picking detalle
  data: les_pos_picking type ZEDSD_PICKING_DET.
*.... recorro el atributo de detalle de reservas
  LOOP AT DETALLES_RESERVAS into les_reservas_dt WHERE movement = 'X'.
    clear:les_pos_picking.
**
          READ table CABECERA_AGLUTINADOR into les_cabe_aglut with key rsnum  = les_reservas_dt-RESERV_NO.
*          IF sy-subrc eq 0.
*            les_pos_picking-aufnr = les_cabe_aglut-NM_REF_P.
*          ENDIF.
           IF sy-subrc eq 0.
            les_pos_picking-aufnr = les_cabe_aglut-NM_REF_h.
          ENDIF.
          les_pos_picking-posicion = les_reservas_dt-RES_ITEM.
          les_pos_picking-MATERIAL = les_reservas_dt-material.
          les_pos_picking-CANTIDAD = les_reservas_dt-ENTRY_QNT.
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
*          les_pos_picking-CANTCONT

*          les_pos_picking-UMCC
*          les_pos_picking-P_CONFIRM
*          les_pos_picking-DOC_ASOCIADO
          les_pos_picking-rsnum = les_reservas_dt-RESERV_NO.
          append les_pos_picking to r_pos_picking.
  ENDLOOP.
endmethod.
ENDCLASS.
