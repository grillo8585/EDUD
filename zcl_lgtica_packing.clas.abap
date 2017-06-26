class ZCL_LGTICA_PACKING definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_PACKING
*"* do not include other source files here!!!
public section.

  data PACKNUM type ZED_PACKNUM .
  data S_PACKING type ZEDSD_PACKING .
  data T_PACKING_DET type ZTTSD_PACKING_POS .
  data T_PACKING_SER type ZTTSD_PACKING_SER .
  data T_PACKING_LOT type ZTTSD_PACKING_LOT .
  data S_PACKING_TR type ZMM_T_TRANSPORTE .
  data T_PACKING_UM type ZMM_TT_TRANSP_DT .

  class-methods SET_CREATE_BY_PAKBLE_REF
    importing
      value(I_REF_PACKABLE) type ref to ZIF_LGTICA_PACKING
      value(I_USUARIO) type ZED_USUARIO_MOVIL optional
    exporting
      value(E_PACKNUM) type ZTTSD_PACKNUM .
  methods LOAD_PACKING
    importing
      !I_PACKNUM type ZED_PACKNUM .
  class-methods CREATE_BY_PAKBLE_REF
    importing
      !I_REF_PACKABLE type ref to ZIF_LGTICA_PACKING
    exporting
      !E_PACKNUM type ZTTSD_PACKNUM .
  class-methods GET_DATA_BY_PAKBLE_REF
    importing
      value(I_REF_PACKABLE) type ref to ZIF_LGTICA_PACKING
    changing
      value(I_INDICADOR) type C optional
      value(C_PACKING_CAB) type ZTTSD_PACKING
      value(C_PACKING_DET) type ZTTSD_PACKING_POS
      value(C_PACKING_LOT) type ZTTSD_PACKING_LOT
      value(C_PACKING_SER) type ZTTSD_PACKING_SER .
  methods UPDATE_PACKING_BY_CANT
    importing
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO optional
      value(I_ENTREGA) type VBELN_VL optional
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL
    changing
      value(C_CAB_POSICIONES) type ZTTSD_PACKING_POS
      value(C_REG_POSICIONES) type ZTTSD_PACKING_POS
      value(C_MENSAJES) type ZTTSD_MSG_PACKING
      value(C_PACKING_DET_SUP) type ZTTSD_PACKING_POS
      value(C_PACKING_SER) type ZTTSD_PACKING_SER
      value(C_PACKING_LOT) type ZTTSD_PACKING_LOT .
  class-methods ADD_SER_LOT
    importing
      value(I_DOCUMENTO_ORIGEN) type ZE_DOCUMENTO optional
      value(I_ENTREGA) type VBELN_VL optional
      value(I_TIPO_DOCUMENTO) type ZE_TIPO_DOC optional
      value(I_CLASE_DOCUMENTO) type ZE_CLAS_DOCUMENTO optional
      value(I_USUARIO) type ZED_USUARIO_MOVIL
      value(C_MENSAJES) type ZTTSD_MSG_PACKING
    changing
      value(C_PACKING_SER) type ZTTSD_PACKING_SER
      value(C_PACKING_LOT) type ZTTSD_PACKING_LOT .
  methods PACK_MATERIAL
    importing
      !I_TKNUM type TKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods GET_DATA_BY_DELIVERY
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
    exporting
      !E_PACKNUM type ZED_PACKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  methods CERRAR_PACKING
    importing
      !I_PACKNUM type ZED_PACKNUM
      !I_USUARIO type ZED_USUARIO_MOVIL .
  class-methods GET_DATA_BY_TKNUM
    importing
      !I_TKNUM type TKNUM
    exporting
      !E_PACKNUM type ZED_PACKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods GET_DATA_BY_RSNUM
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
    exporting
      !E_PACKNUM type ZED_PACKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  methods PACK_MATERIAL_SE
    importing
      !I_TKNUM_SE type TKNUM
      value(I_ENTREGA) type VBELN_VL
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
  class-methods GET_DATA_BY_ORDEN
    importing
      !I_TIPO_DOC type ZE_TIPO_DOC
      !I_DOCUMENTO type ZE_DOCUMENTO
    exporting
      !E_PACKNUM type ZED_PACKNUM
    changing
      !C_MENSAJES type ZTTSD_MSG_PACKING .
protected section.
*"* protected components of class ZCL_LGTICA_PACKING
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_PACKING
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_PACKING IMPLEMENTATION.


method ADD_SER_LOT.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Registros en Tabla de lotes y seriales packing
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Tablas para el manejo de EANS
  data: t_detalle type standard table of  zesd_eanmat,
        t_mensajes type standard table of zesd_msg_packing.
*... Estructuras para el manejo de ENAS
  data: les_detalle type zesd_eanmat.
*... Estructuras para el manejo de las tablas de Seriales y Lotes
  DATA: les_packing_ser TYPE zedsd_packing_ser,
        les_packing_lot TYPE zedsd_packing_lot.
*... Estructuras para el manejo de las tablas de Seriales y Lotes
  DATA: lti_packing_ser TYPE standard table of zmm_t_pack_ser,
        lti_packing_lot TYPE standard table of zmm_t_pack_lot.
*... Variables para setear el valor dependiendo de del tipo de documento
  DATA: l_e_documento_ser TYPE zed_documento,
        l_e_documento_lot TYPE zed_documento,
        l_e_vbeln TYPE  vbeln_va,
        l_e_ebeln TYPE  ebeln ,
        l_e_aglutinador TYPE  zed_aglutinador ,
        l_e_vbeln2  TYPE  vbeln_vl  ,
        l_e_aufnr TYPE  aufnr ,
        l_e_rsnum TYPE  rsnum .
*... Código de barras
  DATA: l_e_cod_bar TYPE zed_cod_bar.
*.... Cantidad contada en el dispositivo para un material UMB
  data: lv_cantcontcab_umb type zedsd_cant_cnt.

  FIELD-SYMBOLS: <fs_pack_ser> TYPE zmm_t_pack_ser,
                 <fs_pack_lot> TYPE zmm_t_pack_lot.

*...Valido que hayan ingresado entrega
  IF i_entrega IS NOT INITIAL.
    l_e_vbeln2 = i_entrega.
  ENDIF.
*... Consulto el primer registro de seriales y guardo el documento asociado
  READ TABLE c_packing_ser INTO les_packing_ser INDEX 1.
  IF sy-subrc EQ 0 .
    l_e_documento_ser = les_packing_ser-documento.
  ENDIF.
*... Consulto el primer registro de lotes y guardo el documento asociado
  READ TABLE c_packing_lot INTO les_packing_lot INDEX 1.
  IF sy-subrc EQ 0 .
    l_e_documento_lot = les_packing_lot-documento.
  ENDIF.
  CASE i_tipo_documento.
*Ventas
    WHEN 'PVTA'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_vbeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_vbeln = l_e_documento_lot.
      ENDIF.
    WHEN 'PROY'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_vbeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_vbeln = l_e_documento_lot.
      ENDIF.
*            Compras
    WHEN 'TRAS'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'SUBC'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'DEVO'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'ICOM'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_ebeln = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_ebeln = l_e_documento_lot.
      ENDIF.
    WHEN 'OPAG'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aglutinador = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aglutinador = l_e_documento_lot.
      ENDIF.
    WHEN 'OPSG'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'OSEX'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'RINV'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_rsnum = l_e_documento_ser.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_rsnum = l_e_documento_lot.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'OSIN'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_aufnr = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_aufnr = l_e_documento_lot.
      ENDIF.
    WHEN 'ENTR'.
      IF l_e_documento_ser IS NOT INITIAL.
        l_e_VBELN2 = l_e_documento_ser.
      ELSEIF l_e_documento_lot IS NOT INITIAL.
        l_e_VBELN2 = l_e_documento_lot.
      ENDIF.
    WHEN 'OSPR'.

  ENDCASE.

*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_vbeln
    IMPORTING
      output = l_e_vbeln.

*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_ebeln
    IMPORTING
      output = l_e_ebeln.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_aglutinador
    IMPORTING
      output = l_e_aglutinador.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_vbeln2
    IMPORTING
      output = l_e_vbeln2.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_aufnr
    IMPORTING
      output = l_e_aufnr.
*.....Función para Ajuste de Número
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_e_rsnum
    IMPORTING
      output = l_e_rsnum.

*... Inserción de seriales y lotes.
  LOOP AT c_packing_ser ASSIGNING <fs_pack_ser>.
    IF l_e_documento_ser IS NOT INITIAL.
      <fs_pack_ser>-documento = l_e_documento_ser.
    ENDIF.
    <fs_pack_ser>-vbeln = l_e_vbeln .
    <fs_pack_ser>-ebeln = l_e_ebeln.
    <fs_pack_ser>-aglutinador = l_e_aglutinador.
    <fs_pack_ser>-vbeln2 = l_e_vbeln2.
    <fs_pack_ser>-aufnr = l_e_aufnr.
    <fs_pack_ser>-rsnum = l_e_rsnum.
*{   REPLACE        ER3K900340                                        1
*\*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
*\    l_e_cod_bar = <fs_pack_ser>-umanipulacion.
*\    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
*\      EXPORTING
*\        i_rotulo = l_e_cod_bar
*\      IMPORTING
*\        e_exidv  = <fs_pack_ser>-umanipulacion.
*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
    IF <fs_pack_ser>-umanipulacion is NOT INITIAL.
      l_e_cod_bar = <fs_pack_ser>-umanipulacion.
    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
      EXPORTING
        i_rotulo = l_e_cod_bar
      IMPORTING
        e_exidv  = <fs_pack_ser>-umanipulacion.
    ENDIF.

*}   REPLACE

*     <fs_pack_ser>-ZED_UNIDAD_MA =
<fs_pack_ser>-tipo_doc = i_tipo_documento.
  ENDLOOP.
*... Valido que se hayan contado Lotes
   IF c_packing_lot[] is not initial.
*... Consulto si los lotes contados se encuentran en BD
*{   REPLACE        ER3K900332                                        4
*\    SELECT *
*\      into corresponding fields of table lti_packing_lot
    SELECT MANDT DOCUMENTO TIPO_DOC MATNR LOTE UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE CANTIDAD UMC VBELN  EBELN AGLUTINADOR VBELN2 AUFNR
           RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
      into table lti_packing_lot
*}   REPLACE
      from zmm_t_pack_lot
      for all entries in c_packing_lot
      where DOCUMENTO eq c_packing_lot-documento and
      TIPO_DOC eq c_packing_lot-tipo_doc and
      MATNR eq c_packing_lot-matnr and
      LOTE eq c_packing_lot-lote and
      UBICACION_FI eq c_packing_lot-UBICACION_FI and
*      UMANIPULACION eq c_packing_lot-UMANIPULACION and
      UEMBALAJE eq c_packing_lot-UEMBALAJE.
  ENDIF.


  LOOP AT c_packing_lot ASSIGNING <fs_pack_lot>.
*... Asigno el documento
    IF l_e_documento_lot IS NOT INITIAL.
      <fs_pack_lot>-documento = l_e_documento_lot.
    ENDIF.

    clear:t_detalle.
*... Consulto los EANS de el material
    call function 'Z_SD_CONSUL_EAN_MATE'
       exporting
         matnr            = <fs_pack_lot>-MATNR
        tables
*       T_MARA           =
          t_detalle        = t_detalle
          t_mensajes       = t_mensajes
                .
    lv_cantcontcab_umb = <fs_pack_lot>-cantidad.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
    read table t_detalle into les_detalle with key unidad_medida = <fs_pack_lot>-umc.
    if sy-subrc eq 0.
      lv_cantcontcab_umb = <fs_pack_lot>-cantidad /  les_detalle-cantidad_ean.
    endif.
*{   REPLACE        ER3K900340                                        2
*\*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
*\    l_e_cod_bar = <fs_pack_lot>-umanipulacion.
*\    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
*\      EXPORTING
*\        i_rotulo = l_e_cod_bar
*\      IMPORTING
*\        e_exidv  = <fs_pack_lot>-umanipulacion.
*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
    IF <fs_pack_lot>-umanipulacion is not INITIAL.
      l_e_cod_bar = <fs_pack_lot>-umanipulacion.
    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
      EXPORTING
        i_rotulo = l_e_cod_bar
      IMPORTING
        e_exidv  = <fs_pack_lot>-umanipulacion.
    ENDIF.

*}   REPLACE

*... Leo si existe el lote en BD
*{   REPLACE        ER3K900340                                        3
*\     READ TABLE lti_packing_lot into les_packing_lot with key documento = <fs_pack_lot>-documento
*\     tipo_doc = <fs_pack_lot>-tipo_doc matnr = <fs_pack_lot>-matnr lote = <fs_pack_lot>-lote umanipulacion = <fs_pack_lot>-umanipulacion.
     READ TABLE lti_packing_lot into les_packing_lot with key documento = <fs_pack_lot>-documento
     tipo_doc = <fs_pack_lot>-tipo_doc matnr = <fs_pack_lot>-matnr lote = <fs_pack_lot>-lote umanipulacion = <fs_pack_lot>-umanipulacion
     UMANIPULACION_SE = <fs_pack_lot>-UMANIPULACION_SE.
*}   REPLACE
*... Acumulo la cantidad
     IF sy-subrc eq 0 .
        lv_cantcontcab_umb = lv_cantcontcab_umb + les_packing_lot-cantidad.
      ENDIF.

    <fs_pack_lot>-cantidad = lv_cantcontcab_umb.
    <fs_pack_lot>-cantidad_uso = lv_cantcontcab_umb.
    <fs_pack_lot>-umc_uso = <fs_pack_lot>-umc_uso.
    <fs_pack_lot>-tipo_doc = i_tipo_documento.
    <fs_pack_lot>-vbeln = l_e_vbeln .
    <fs_pack_lot>-ebeln = l_e_ebeln.
    <fs_pack_lot>-aglutinador = l_e_aglutinador.
    <fs_pack_lot>-vbeln2 = l_e_vbeln2.
    <fs_pack_lot>-aufnr = l_e_aufnr.
    <fs_pack_lot>-rsnum = l_e_rsnum.
  ENDLOOP.


  MODIFY  zmm_t_pack_ser FROM TABLE c_packing_ser.
  MODIFY  zmm_t_pack_lot FROM TABLE c_packing_lot.
endmethod.


method CERRAR_PACKING.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Carga los datos de un documento packing
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tabla interna para la cabecera del documento de transporte
  data: lti_transporte type standard table of zmm_t_transporte.
*... Estructura para la cabecera del documento de transporte
  data: les_transporte type zmm_t_transporte.
*... tabla interna para la posicion del documento de transporte
  data: lti_transporte_dt type standard table of zmm_t_transp_dt.
*... Estructura para la posicion del documento de transporte
  data: les_transporte_dt type zmm_t_transp_dt.
*{   INSERT         ER3K900332                                        6
*... Variable para documento de transporte
  data: lv_documento type ZE_DOCUMENTO.
*}   INSERT

*... tabla interna para la cabecera del documento de packing
  data: lti_packing type standard table of zmm_t_packing.
*... Estructura para la cabecera del documento de packing
  data: les_packing type zmm_t_packing.
*... tabla interna para la posicion del documento de packing
  data: lti_packing_dt type standard table of zmm_t_packing_po.
*... Estructura interna para la posicion del documento de packing
  data: les_packing_dt type  zmm_t_packing_po.
*... Variable para llevar el consecutivo de las posiciones del documento de packing
  data: lv_consecutivo type zed_pospack.
*... Valido que se haya ingresado un numero de documento de packing
  if i_packnum is not initial.
*.... Consulto la cabecera para los documento de packing
*{   REPLACE        ER3K900332                                        1
*\      select *  from zmm_t_packing
      select MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
          from zmm_t_packing
*}   REPLACE
        into table lti_packing
        where packnum eq i_packnum.
*... valido que hayan encontrado registros de cabecera
        if lti_packing[] is not initial.
*... cargo en la estructura el registro encontrado de la cabecera del documento de packing
            read table lti_packing into les_packing index 1.
            if sy-subrc eq 0.
              les_packing-estado = 'CERRADO'.
              les_packing-femod  = sy-datum.
              les_packing-homod  = sy-uzeit.
              les_packing-usrmod = i_usuario.
              les_packing-unamod = sy-uname.

*...  Actualizo el registro de cabecera
              modify  zmm_t_packing from les_packing .
*... Realizo set del atributo de cabecera del documento de packing
              s_packing = les_packing.
            endif.
*... Consulto el detalle para los documento de packing encontrado
*{   REPLACE        ER3K900332                                        2
*\          select  * from zmm_t_packing_po
          select  MANDT POSICION PACKNUM POSPACK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT
                  UMCC P_CONFIRM  DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO UMANIPULACION UMANIPULACION_SE UEMBALAJE
             from zmm_t_packing_po
*}   REPLACE
            into table lti_packing_dt
            for all entries in lti_packing
            where packnum eq lti_packing-packnum.
*... valido que hayan encontrado registros de detalle del documento de packing
            if lti_packing_dt[] is not initial.
*... Consulto el transporte asociado al packing ( entrega)
*{   REPLACE        ER3K900332                                        5
*\              select *
*\                into CORRESPONDING FIELDS OF TABLE lti_transporte
*\                from zmm_t_transporte
*\                where documento eq les_packing-vbeln2.
                IF les_packing-vbeln2 is not INITIAL.
                  select MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                  into TABLE lti_transporte
                  from zmm_t_transporte
                  where documento eq les_packing-vbeln2.
                ELSEIF les_packing-aufnr is not INITIAL.
                  select MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                  into TABLE lti_transporte
                  from zmm_t_transporte
                  where documento eq les_packing-aufnr.
                ELSEIF les_packing-rsnum is not INITIAL.
*.....Función para Ajuste de Número
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = les_packing-rsnum
                    IMPORTING
                      output = lv_documento.

                  select MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
                  into TABLE lti_transporte
                  from zmm_t_transporte
                  where documento eq lv_documento.
                ENDIF.


*}   REPLACE
*... Valido la respuesta de la consulta
              IF sy-subrc eq 0.
*... Consulto las unidades de manipulación
*{   REPLACE        ER3K900332                                        4
*\                select *
*\                  into CORRESPONDING FIELDS OF TABLE lti_transporte_dt
                select MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
                  into TABLE lti_transporte_dt
*}   REPLACE
                  from zmm_t_transp_dt
                  FOR ALL ENTRIES IN lti_transporte
                  where tknum eq lti_transporte-tknum.
*... Valido la consulta
                  IF sy-subrc eq 0.
                    LOOP AT lti_transporte_dt into les_transporte_dt.
                      clear:les_packing_dt.
*... Valido si existe la unidad de manipulación en el detalle ( esta o no vacía)
                      READ TABLE lti_packing_dt into les_packing_dt WITH KEY UMANIPULACION = les_transporte_dt-EXIDV.
                      IF les_packing_dt is initial.
*... Elimino la unidad de manipulación que no se Uso
                        DELETE FROM zmm_t_transp_dt where EXIDV = les_transporte_dt-EXIDV.
                      ENDIF.

                    ENDLOOP.
                  ENDIF.

              ENDIF.
*... Depuro los registros que no se les ha realizado algun conteo

              delete lti_packing_dt where cantcont = 0.

              clear:lv_consecutivo.

              loop at lti_packing_dt into les_packing_dt.
                lv_consecutivo = lv_consecutivo + 1.
                les_packing_dt-pospack = lv_consecutivo.
                les_packing_dt-cantidad = les_packing_dt-cantcont.
                les_packing_dt-p_confirm = 'CT'.
                modify lti_packing_dt from les_packing_dt index sy-tabix.
              endloop.
*... Borro todos los registros de la tabla
              delete from zmm_t_packing_po where packnum = les_packing-packnum .
*... Actualizo la tabla Z de detalle de packing con los registros que fueron filtrados
                modify zmm_t_packing_po from table lti_packing_dt.
*... Realizo set del atributo de detalle del documento de packing
              t_packing_det = lti_packing_dt.
*... Consulto los seriales para los detalles del documento de packing
            endif.
        endif.
  endif.
commit WORK AND WAIT.
endmethod.


METHOD create_by_pakble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Packing
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de packing
  DATA: r_header_packing TYPE zttsd_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_header_packing TYPE zedsd_packing.


*... tabla interna para la cabecera del documento de packing
  DATA: lti_packing TYPE STANDARD TABLE OF zmm_t_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_packing TYPE zmm_t_packing.
*... tabla interna para la posicion del documento de packing
  DATA: lti_packing_dt TYPE STANDARD TABLE OF zmm_t_packing_po.


*... Cabecera de posicion documento de packing
  DATA: r_pos_packing TYPE zttsd_packing_pos.
*... Estructura para posición del documento de packing
  DATA: les_pos_packing TYPE zedsd_packing_pos.

  DATA: lv_consecutivo TYPE zed_pospack.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPACKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado


*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de packing
  CALL METHOD i_ref_packable->get_header_packing
    RECEIVING
      r_header_packing = r_header_packing.

*... Válido que existan registros
  IF r_header_packing[] IS NOT INITIAL.

*... Consulto todos los registros que se hayan creado con anterioridad
*{   REPLACE        ER3K900332                                        9
*\    SELECT *
*\      FROM zmm_t_packing
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_packing
    SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_packing
      INTO  TABLE lti_packing
*}   REPLACE
      FOR ALL ENTRIES IN  r_header_packing
      WHERE vbeln2 EQ r_header_packing-vbeln2
      AND vbeln EQ r_header_packing-vbeln
      AND ebeln EQ r_header_packing-ebeln
      AND aglutinador EQ r_header_packing-aglutinador
      AND aufnr EQ r_header_packing-aufnr
      AND rsnum EQ r_header_packing-rsnum.
*... Llamo el metodo para Retornara la Estructura de detalle para un document de packing
    CALL METHOD i_ref_packable->get_pos_packing
      RECEIVING
        r_pos_packing = r_pos_packing.

    IF lti_packing IS NOT INITIAL.
      "Filtro los documentos de packing cerrados
      LOOP AT lti_packing INTO les_packing.
*{   REPLACE        ER3K900332                                        8
*\        IF les_packing-estado = 'CERRADO' AND ( les_packing-aufnr IS INITIAL AND les_packing-rsnum IS INITIAL ).
        IF les_packing-estado = 'CERRADO' .
*}   REPLACE
*... se borra registro cerrado de la cabecera
          DELETE r_header_packing WHERE vbeln2 = les_packing-vbeln2 AND
                             vbeln  = les_packing-vbeln AND
                             ebeln  = les_packing-ebeln AND
                             aglutinador = les_packing-aglutinador AND
                             aufnr = les_packing-aufnr AND
                             rsnum = les_packing-rsnum.
*... Se borran los registros de posición del documento cerrado
          DELETE r_pos_packing WHERE packnum = les_packing-packnum .

        ENDIF.
      ENDLOOP.

    ENDIF.

*... Recorro las cabeceras de packing
    LOOP AT r_header_packing INTO les_header_packing.
      CLEAR:    lv_consecutivo ,les_packing.
*... Elimino los lotes y seriales del documento de packing
*{   REPLACE        ER3K900332                                        3
*\     DELETE from zmm_t_pack_ser where VBELN2 = les_packing-VBELN2 .
*\     DELETE FROM zmm_t_pack_lot where VBELN2 = les_packing-VBELN2 .
     IF les_header_packing-vbeln2 is not INITIAL.
       DELETE from zmm_t_pack_ser where VBELN2 = les_header_packing-vbeln2 .
       DELETE FROM zmm_t_pack_lot where VBELN2 = les_header_packing-vbeln2 .
     ELSEIF les_header_packing-aufnr is not INITIAL.
       DELETE from zmm_t_pack_ser where aufnr = les_header_packing-aufnr .
       DELETE FROM zmm_t_pack_lot where aufnr = les_header_packing-aufnr .
     ELSEIF les_header_packing-rsnum is not INITIAL.
       DELETE from zmm_t_pack_ser where rsnum = les_header_packing-rsnum .
       DELETE FROM zmm_t_pack_lot where rsnum = les_header_packing-rsnum .
     ENDIF.

*}   REPLACE

*.... Busco si hay algun registro en la cabecera

      READ TABLE lti_packing INTO les_packing WITH KEY vbeln2 = les_header_packing-vbeln2 vbeln = les_header_packing-vbeln ebeln = les_header_packing-ebeln
       aglutinador = les_header_packing-aglutinador aufnr = les_header_packing-aufnr rsnum = les_header_packing-rsnum.
      IF sy-subrc EQ 0 .

        MOVE les_packing TO les_header_packing.
*{   DELETE         ER3K900332                                        2
*\        IF les_packing-aufnr IS INITIAL AND les_packing-rsnum IS INITIAL.
*}   DELETE
*... Se borran los registros de posición del documento cerrado
          DELETE FROM zmm_t_packing_po WHERE packnum = les_header_packing-packnum .
*{   DELETE         ER3K900332                                        1
*\        ELSE.
*\*... Asigno estado pendiente otra vez
*\          les_header_packing-estado = 'PENDIENTE'.
*\          SELECT SINGLE MAX( pospack )
*\            FROM zmm_t_packing_po
*\            INTO lv_consecutivo
*\            WHERE packnum = les_header_packing-packnum.
*\
*\
*\*... Elimino los lotes y seriales del documento de packing
*\          DELETE FROM zmm_t_pack_ser WHERE vbeln2 = les_header_packing-vbeln2 .
*\          DELETE FROM zmm_t_pack_lot WHERE vbeln2 = les_header_packing-vbeln2 .
*\        ENDIF.
*}   DELETE

      ELSE.
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

        les_header_packing-packnum = p_doc_number.
        les_header_packing-estado = 'PENDIENTE'.

      ENDIF.

*{   REPLACE        ER3K900332                                        7
*\      LOOP AT r_pos_packing INTO les_pos_packing WHERE vbeln2 = les_header_packing-vbeln2 AND rsnum = les_header_packing-rsnum AND cantidad > 0.
      LOOP AT r_pos_packing INTO les_pos_packing WHERE vbeln2 = les_header_packing-vbeln2 AND rsnum = les_header_packing-rsnum
                                                   AND aufnr = les_header_packing-aufnr  AND cantidad > 0.
*}   REPLACE
        lv_consecutivo = lv_consecutivo + 1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = les_pos_packing-material
          IMPORTING
            output = les_pos_packing-material.
*AD
        les_pos_packing-cantidad =   les_pos_packing-cantidad - les_pos_packing-cantcont.
*AD
        les_pos_packing-packnum = les_header_packing-packnum.
*{   REPLACE        ER3K900332                                        6
*\        les_pos_packing-pospack = lv_consecutivo.
        IF les_pos_packing-pospack is INITIAL.
          les_pos_packing-pospack = lv_consecutivo.
        ENDIF.
          les_pos_packing-umanipulacion_se = les_pos_packing-pospack.
*}   REPLACE
*MD          les_pos_packing-diferencia = les_pos_packing-cantidad - les_pos_packing-CANTCONT.
        les_pos_packing-diferencia = les_pos_packing-cantidad.
        les_pos_packing-cantcont = 0.
        APPEND les_pos_packing TO lti_packing_dt.
      ENDLOOP.

      MODIFY zmm_t_packing FROM les_header_packing.
      MODIFY zmm_t_packing_po FROM TABLE lti_packing_dt.
    ENDLOOP.

  ENDIF.
ENDMETHOD.


METHOD get_data_by_delivery.

*  Estructura para el manejo de mensajes.
  DATA: les_mensajes TYPE zesd_msg_packing,
        lv_vbeln     TYPE vbeln.
* Función que saca los ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
 EXPORTING
  INPUT = i_documento
 IMPORTING
  OUTPUT = lv_vbeln.

* Función que completa con ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = lv_vbeln
 IMPORTING
  OUTPUT = lv_vbeln.

*  lv_vbeln = i_documento.
*  Obtenemos el packnum de la tabal de cabecera de packing.
  SELECT SINGLE packnum
    INTO e_packnum
    FROM zmm_t_packing
    WHERE vbeln2 EQ lv_vbeln.
*  En caso de que no se encuentre el packnum se retorna mensaje de error.
  IF sy-subrc NE 0.
    les_mensajes-num_doc = lv_vbeln.
*{   REPLACE        ER3K900332                                        1
*\    CONCATENATE 'No existen datos de cabecera para documento' lv_vbeln
*\      INTO les_mensajes-msg SEPARATED BY space.
     MESSAGE S013(ZCLPACK) WITH lv_vbeln into les_mensajes-msg.
*}   REPLACE
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

ENDMETHOD.


  method GET_DATA_BY_ORDEN.
*{   INSERT         ER3K900332                                        1

*  Estructura para el manejo de mensajes.
  DATA: les_mensajes TYPE zesd_msg_packing,
        lv_aufnr     TYPE aufnr.
* Función que saca los ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
 EXPORTING
  INPUT = i_documento
 IMPORTING
  OUTPUT = lv_aufnr.

* Función que completa con ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = lv_aufnr
 IMPORTING
  OUTPUT = lv_aufnr.


*  Obtenemos el packnum de la tabal de cabecera de packing.
  SELECT SINGLE packnum
    INTO e_packnum
    FROM zmm_t_packing
    WHERE aufnr EQ lv_aufnr.
*  En caso de que no se encuentre el packnum se retorna mensaje de error.
  IF sy-subrc NE 0.
    les_mensajes-num_doc = lv_aufnr.
   MESSAGE S013(ZCLPACK) WITH lv_aufnr into les_mensajes-msg .
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

*}   INSERT
  endmethod.


METHOD get_data_by_pakble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Consultar Cabeceras y Detalles de Documentos Packing
* Autor Prog.  :
* Fecha Creac. : 21.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  DATA:
*{   INSERT         ER3K900340                                        4
*        Manejo de eans
      lti_mara TYPE TABLE OF mara,
      les_mara TYPE mara,

      l_e_documento type ZE_DOCUMENTO,
      r_documento type range of zmm_t_transporte-documento,  "range table
      wa_documento like line of r_documento,     "work area for range table
*  Datos tablas Z.
        lti_transporte    TYPE TABLE OF zmm_t_transporte,
        les_transporte    TYPE zmm_t_transporte,
        les_packing type ZEDSD_PACKING,
*}   INSERT
*.....Tablas de Retorno de Función para Busqueda de EANs Material
           lti_detalle TYPE TABLE OF zesd_eanmat,
           lti_mensajes TYPE TABLE OF zesd_msg_packing,
*.....Tabla Interna con informacion de Documentos Packing
         lti_header_packing TYPE zttsd_packing.
*... Código de barras
  DATA: l_e_cod_bar TYPE zed_cod_bar.
*.... Estructura  asignar al field symbol para la entrega
  DATA: les_entrega_picking TYPE zesd_entrega_picking.
*.... Estructura  asignar al field symbol de posiciónes de un documento de packing
  DATA: les_pos_packing TYPE zedsd_packing_pos.
  FIELD-SYMBOLS:
*.... field symbol para los seriales
        <lfs_packing_ser> TYPE zedsd_packing_ser,
*.... field symbol para los lotes
        <lfs_packing_lot> TYPE zedsd_packing_lot,
*.... field symbol para los EANS
        <lfs_detalle> TYPE zesd_eanmat,
*.....Estructura Tabla Interna Packing detalle
       <lfs_pos_packing> TYPE zedsd_packing_pos,
*.....Estructura Tabla Interna Packing
       <lfs_header_packing> TYPE zedsd_packing.
**....Estructura con Entregas-Packing
*         <lfs_entrega_packing> type zesd_entrega_picking.
**... Asigno estructuras a los fieldsymbol
*    assign les_entrega_picking to <lfs_entrega_picking>.

*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de packing
  CALL METHOD i_ref_packable->get_header_packing
    RECEIVING
      r_header_packing = lti_header_packing.
*... Válido que existan registros
  IF lti_header_packing[] IS NOT INITIAL.
*.....Ordeno Tabla a Procesar
    SORT lti_header_packing BY vbeln ebeln aglutinador ASCENDING.
*.....Consulto Tabla Cabeceras Packing
*{   REPLACE        ER3K900332                                        9
*\    SELECT *
*\        FROM zmm_t_packing
*\            INTO  CORRESPONDING FIELDS OF TABLE c_packing_cab
*\           FOR ALL ENTRIES IN  lti_header_packing
*\                       WHERE vbeln EQ lti_header_packing-vbeln AND
*\                             ebeln EQ lti_header_packing-ebeln AND
*\                             aglutinador EQ lti_header_packing-aglutinador AND
*\                             vbeln2 EQ lti_header_packing-vbeln2 AND
*\                             aufnr EQ lti_header_packing-aufnr AND
*\                             rsnum EQ lti_header_packing-rsnum.
    SELECT MANDT PACKNUM  PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
        FROM zmm_t_packing
            INTO TABLE c_packing_cab
           FOR ALL ENTRIES IN  lti_header_packing
                       WHERE vbeln EQ lti_header_packing-vbeln AND
                             ebeln EQ lti_header_packing-ebeln AND
                             aglutinador EQ lti_header_packing-aglutinador AND
                             vbeln2 EQ lti_header_packing-vbeln2 AND
                             aufnr EQ lti_header_packing-aufnr AND
                             rsnum EQ lti_header_packing-rsnum.
*}   REPLACE

*.....Indicador
    i_indicador = 'X'.

    IF sy-subrc EQ 0 AND i_indicador IS NOT INITIAL.
*.....Ordeno la Tabla a Procesar
      SORT c_packing_cab BY packnum ASCENDING.
*{   INSERT         ER3K900332                                        8
      LOOP AT c_packing_cab into les_packing.
          wa_documento-sign = 'I'.
          wa_documento-option = 'EQ'.
          IF les_packing-aufnr is not INITIAL.
              wa_documento-low = les_packing-aufnr.
              APPEND wa_documento to r_documento.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input         = les_packing-aufnr
               IMPORTING
                 OUTPUT        = wa_documento-low
                           .
              APPEND wa_documento to r_documento.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = les_packing-aufnr
               IMPORTING
                 OUTPUT        = wa_documento-low
                           .
          ELSEIF les_packing-rsnum is not INITIAL.
            wa_documento-low = les_packing-rsnum.
              APPEND wa_documento to r_documento.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input         = les_packing-rsnum
               IMPORTING
                 OUTPUT        = wa_documento-low.
            APPEND wa_documento to r_documento.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = les_packing-rsnum
               IMPORTING
                 OUTPUT        = wa_documento-low.
          ELSEIF les_packing-vbeln2 is not INITIAL.
            wa_documento-low = les_packing-vbeln2.
              APPEND wa_documento to r_documento.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input         = les_packing-vbeln2
               IMPORTING
                 OUTPUT        = wa_documento-low.
            APPEND wa_documento to r_documento.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = les_packing-vbeln2
               IMPORTING
                 OUTPUT        = wa_documento-low.
          ENDIF.
          APPEND wa_documento to r_documento.
      ENDLOOP.
      IF r_documento is not INITIAL.
        SELECT MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
          into table lti_transporte
          from ZMM_T_TRANSPORTE
          where documento in r_documento.
      ENDIF.

*}   INSERT
*.....Consulto Tabla Posiciones Packing
*{   REPLACE        ER3K900332                                       10
*\      SELECT *
*\         FROM zmm_t_packing_po
*\            INTO CORRESPONDING FIELDS OF TABLE c_packing_det
*\                FOR ALL ENTRIES IN  c_packing_cab
*\                    WHERE packnum EQ c_packing_cab-packnum.
      SELECT MANDT POSICION PACKNUM POSPACK  MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO
             CANTCONT UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO UMANIPULACION UMANIPULACION_SE UEMBALAJE
         FROM zmm_t_packing_po
            INTO TABLE c_packing_det
                FOR ALL ENTRIES IN  c_packing_cab
                    WHERE packnum EQ c_packing_cab-packnum.
*}   REPLACE
      IF sy-subrc EQ 0.
*{   INSERT         ER3K900332                                       13
         LOOP AT c_packing_det ASSIGNING <lfs_pos_packing>.
            les_mara-matnr = <lfs_pos_packing>-material.
            APPEND les_mara TO lti_mara.
         ENDLOOP.

           CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
            TABLES
              t_detalle  = lti_detalle
              t_mara = lti_mara.

*}   INSERT
        LOOP AT c_packing_det ASSIGNING <lfs_pos_packing>.
*.....Limpieza de Variables
*{   REPLACE        ER3K900332                                       14
*\          CLEAR : lti_detalle, lti_mensajes,l_e_cod_bar.
          CLEAR : lti_mensajes,l_e_cod_bar.
*}   REPLACE
*{   DELETE         ER3K900332                                       15
*\*.....Función para Obtener EANs de un Material
*\          CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
*\            EXPORTING
*\              matnr      = <lfs_pos_packing>-material
*\            TABLES
*\              t_detalle  = lti_detalle
*\              t_mensajes = lti_mensajes.
*}   DELETE

          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
           EXPORTING
             input                = <lfs_pos_packing>-umc
             language             = 'S'
          IMPORTING
*               LONG_TEXT            =
            output               = <lfs_pos_packing>-umc
*               SHORT_TEXT           =
          EXCEPTIONS
            unit_not_found       = 1
            OTHERS               = 2.

*{   REPLACE        ER3K900332                                       16
*\          READ TABLE  lti_detalle ASSIGNING <lfs_detalle> WITH KEY unidad_medida = <lfs_pos_packing>-umc.
          READ TABLE  lti_detalle ASSIGNING <lfs_detalle> WITH KEY matnr = <lfs_pos_packing>-material unidad_medida = <lfs_pos_packing>-umc.
*}   REPLACE
          IF sy-subrc NE 0 .
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
             EXPORTING
               input                = <lfs_pos_packing>-umc
               language             = 'S'
            IMPORTING
*               LONG_TEXT            =
              output               = <lfs_pos_packing>-umc
*               SHORT_TEXT
            EXCEPTIONS
              unit_not_found       = 1
              OTHERS               = 2
                     .
          ENDIF.

          IF <lfs_pos_packing>-umc EQ '004'.
            <lfs_pos_packing>-umc = 'UN'.
          ENDIF.
          <lfs_pos_packing>-umd = <lfs_pos_packing>-umc.
          <lfs_pos_packing>-umcc = <lfs_pos_packing>-umc.
*{   REPLACE        ER3K900340                                        5
*\
*\          CALL METHOD zcl_lgtica_embalaje=>get_rotulo
*\            EXPORTING
*\              i_exidv                         = <lfs_pos_packing>-umanipulacion
*\            IMPORTING
*\              e_rotulo                        = l_e_cod_bar.
*\              <lfs_pos_packing>-umanipulacion = l_e_cod_bar.
*\
*\            CALL METHOD zcl_lgtica_embalaje=>get_rotulo_se
*\            EXPORTING
*\              i_venum_se                         = <lfs_pos_packing>-umanipulacion_se
*\            IMPORTING
*\              e_rotulo                        = l_e_cod_bar.
*\              <lfs_pos_packing>-umanipulacion_se = l_e_cod_bar.
          IF <lfs_pos_packing>-umanipulacion is not INITIAL.
            CALL METHOD zcl_lgtica_embalaje=>get_rotulo
            EXPORTING
              i_exidv                         = <lfs_pos_packing>-umanipulacion
            IMPORTING
              e_rotulo                        = l_e_cod_bar.

              <lfs_pos_packing>-umanipulacion = l_e_cod_bar.

          ENDIF.

*}   REPLACE



        ENDLOOP.

**.....Lleno Tabla Entregas-Packing
*          loop at c_packing_cab assigning <lfs_header_packing>.
*            <lfs_entrega_packing>-rsnum = <lfs_header_packing>-rsnum .
*            <lfs_entrega_packing>-aufnr = <lfs_header_packing>-aufnr .
*            <lfs_entrega_packing>-vbeln2 = <lfs_header_packing>-vbeln2 .
*            <lfs_entrega_packing>-packnum =  <lfs_header_packing>-picknum.
*            append  <lfs_entrega_packing>  to c_entrega_packing.
*          endloop.
*        endif.

*... Consulto los seriales para los detalles del documento de packing
*{   REPLACE        ER3K900332                                       11
*\        SELECT * FROM zmm_t_pack_ser
*\          INTO CORRESPONDING FIELDS OF TABLE c_packing_ser
*\          FOR ALL ENTRIES IN c_packing_cab
*\*          WHERE picknum EQ i_picknum
*\          WHERE    ( vbeln2 EQ c_packing_cab-vbeln2 AND vbeln2 NE '') OR
*\*                   ( ebeln eq c_packing_cab-ebeln and ebeln ne '') or
*\                   ( aglutinador EQ c_packing_cab-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ c_packing_cab-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ c_packing_cab-rsnum AND rsnum NE '').
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR  SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER UBICACION_FI UMANIPULACION UMANIPULACION_SE
               UEMBALAJE
          FROM zmm_t_pack_ser
          INTO TABLE c_packing_ser

          WHERE    ( documento IN r_documento ).
*}   REPLACE
        IF sy-subrc EQ 0.

          LOOP AT c_packing_ser ASSIGNING <lfs_packing_ser>.
            CLEAR l_e_cod_bar.
*{   REPLACE        ER3K900340                                        6
*\            CALL METHOD zcl_lgtica_embalaje=>get_rotulo
*\              EXPORTING
*\                i_exidv  = <lfs_packing_ser>-umanipulacion
*\              IMPORTING
*\                e_rotulo = l_e_cod_bar.
*\            <lfs_packing_ser>-umanipulacion = l_e_cod_bar.
*\
*\              CALL METHOD zcl_lgtica_embalaje=>get_rotulo_se
*\            EXPORTING
*\              i_venum_se                         = <lfs_packing_ser>-umanipulacion_se
*\            IMPORTING
*\              e_rotulo                        = l_e_cod_bar.
*\              <lfs_packing_ser>-umanipulacion_se = l_e_cod_bar.
         IF <lfs_packing_ser>-umanipulacion is not INITIAL.
           CALL METHOD zcl_lgtica_embalaje=>get_rotulo
              EXPORTING
                i_exidv  = <lfs_packing_ser>-umanipulacion
              IMPORTING
                e_rotulo = l_e_cod_bar.
            <lfs_packing_ser>-umanipulacion = l_e_cod_bar.

         ENDIF.

*}   REPLACE

          ENDLOOP.
        ENDIF.
*{   REPLACE        ER3K900332                                       12
*\*... Consulto los lotes para los detalles del documento de packing
*\        SELECT * FROM zmm_t_pack_lot
*\          INTO CORRESPONDING FIELDS OF TABLE c_packing_lot
*\          FOR ALL ENTRIES IN c_packing_cab
*\*          WHERE packnum EQ i_packnum
*\          WHERE   ( vbeln2 EQ c_packing_cab-vbeln2 AND vbeln2 NE '') OR
*\*                   ( ebeln eq c_packing_cab-ebeln and ebeln ne '') or
*\                   ( aglutinador EQ c_packing_cab-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ c_packing_cab-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ c_packing_cab-rsnum AND rsnum NE '').
*... Consulto los lotes para los detalles del documento de packing
        SELECT MANDT DOCUMENTO  TIPO_DOC MATNR LOTE UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE CANTIDAD UMC VBELN EBELN AGLUTINADOR
               VBELN2 AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
           FROM zmm_t_pack_lot
          INTO TABLE c_packing_lot
          WHERE    ( documento IN r_documento ).
*          FOR ALL ENTRIES IN c_packing_cab
*          WHERE packnum EQ i_packnum
*          WHERE   ( vbeln2 EQ c_packing_cab-vbeln2 AND vbeln2 NE '') OR
**                   ( ebeln eq c_packing_cab-ebeln and ebeln ne '') or
*                   ( aglutinador EQ c_packing_cab-aglutinador AND aglutinador NE '') OR
*                   ( aufnr EQ c_packing_cab-aufnr AND aufnr NE '' ) OR
*                   ( rsnum EQ c_packing_cab-rsnum AND rsnum NE '').
*}   REPLACE
        IF sy-subrc EQ 0.

          LOOP AT c_packing_lot ASSIGNING <lfs_packing_lot>.
            CLEAR l_e_cod_bar.
*{   REPLACE        ER3K900340                                        7
*\            CALL METHOD zcl_lgtica_embalaje=>get_rotulo
*\              EXPORTING
*\                i_exidv  = <lfs_packing_lot>-umanipulacion
*\              IMPORTING
*\                e_rotulo = l_e_cod_bar.
*\            <lfs_packing_lot>-umanipulacion = l_e_cod_bar.
*\
*\            CALL METHOD zcl_lgtica_embalaje=>get_rotulo_se
*\            EXPORTING
*\              i_venum_se                         = <lfs_packing_lot>-umanipulacion_se
*\            IMPORTING
*\              e_rotulo                        = l_e_cod_bar.
*\              <lfs_packing_lot>-umanipulacion_se = l_e_cod_bar.
        IF <lfs_packing_lot>-umanipulacion is NOT INITIAL.
          CALL METHOD zcl_lgtica_embalaje=>get_rotulo
              EXPORTING
                i_exidv  = <lfs_packing_lot>-umanipulacion
              IMPORTING
                e_rotulo = l_e_cod_bar.
            <lfs_packing_lot>-umanipulacion = l_e_cod_bar.

        ENDIF.

*}   REPLACE
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD GET_DATA_BY_RSNUM.
*** inactive new ***
*{   INSERT         ER3K900332                                        6

*  Estructura para el manejo de mensajes.
  DATA: les_mensajes TYPE zesd_msg_packing,
        lv_rsnum     TYPE rsnum.
* Función que saca los ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
 EXPORTING
  INPUT = i_documento
 IMPORTING
  OUTPUT = lv_rsnum.

* Función que completa con ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = lv_rsnum
 IMPORTING
  OUTPUT = lv_rsnum.


*  Obtenemos el packnum de la tabal de cabecera de packing.
  SELECT SINGLE packnum
    INTO e_packnum
    FROM zmm_t_packing
    WHERE rsnum EQ lv_rsnum.
*  En caso de que no se encuentre el packnum se retorna mensaje de error.
  IF sy-subrc NE 0.
    les_mensajes-num_doc = lv_rsnum.
    MESSAGE S013(ZCLPACK) WITH lv_rsnum into les_mensajes-msg.
    les_mensajes-type_msg = 'E'.
    APPEND les_mensajes TO c_mensajes.
  ENDIF.

ENDMETHOD.

*}   INSERT


method GET_DATA_BY_TKNUM.
  data: les_transporte type zmm_t_transporte.

*{   REPLACE        ER3K900332                                        2
*\  select single *
  select single MANDT TKNUM TKNUM_SE  DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
*}   REPLACE
    into les_transporte
    from zmm_t_transporte
    where tknum eq i_tknum.

IF sy-subrc eq 0.
*  Estructura para el manejo de mensajes.
  data: les_mensajes type zesd_msg_packing.
  data: lv_vbeln type vbeln.
*.....Función para Ajuste de Número
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = les_transporte-DOCUMENTO
          importing
            output = lv_vbeln.

*  Obtenemos el packnum de la tabal de cabecera de packing.
  select single packnum
    into e_packnum
    from zmm_t_packing
    where vbeln2 eq lv_vbeln.
*  En caso de que no se encuentre el packnum se retorna mensaje de error.
  if sy-subrc ne 0.
*{   REPLACE        ER3K900332                                        4
*\    les_mensajes-num_doc = i_tknum.
*\    concatenate 'No existen datos de cabecera para documento' i_tknum
*\      into les_mensajes-msg separated by space.
*\    les_mensajes-type_msg = 'E'.
*\    append les_mensajes to c_mensajes.
data: lv_rsnum type rsnum.
*.....Función para Ajuste de Número
         call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = les_transporte-DOCUMENTO
          importing
            output = lv_rsnum.

*  Obtenemos el packnum de la tabal de cabecera de packing.
  select single packnum
    into e_packnum
    from zmm_t_packing
    where rsnum eq lv_rsnum.

    IF sy-subrc ne 0.
        data: lv_aufnr type aufnr.
*        .....Función para Ajuste de Número
                 call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = les_transporte-DOCUMENTO
                  importing
                    output = lv_aufnr.

*          Obtenemos el packnum de la tabal de cabecera de packing.
          select single packnum
            into e_packnum
            from zmm_t_packing
            where aufnr eq lv_aufnr.
            IF sy-subrc ne 0.
                MESSAGE S013(ZCLPACK) WITH i_tknum into les_mensajes-msg.
                    les_mensajes-type_msg = 'E'.
                append les_mensajes to c_mensajes.
            ENDIF.
    ENDIF.

*}   REPLACE
  endif.
ENDIF.

endmethod.


METHOD load_packing.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Carga de un documento de Packing
* Autor Prog.  :
* Fecha Creac. : 24.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... tabla interna para la cabecera del documento de packing
  DATA: lti_packing TYPE STANDARD TABLE OF zmm_t_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_packing TYPE zmm_t_packing.
*... tabla interna para la posicion del documento de packing
  DATA: lti_packing_po TYPE STANDARD TABLE OF zmm_t_packing_po.
*... Estructura interna para la posicion del documento de pocking
  DATA: les_packing_po TYPE  zmm_t_packing_po.

*... tabla interna para los seriales de un documento de packing
  DATA: lti_packing_ser TYPE STANDARD TABLE OF zmm_t_pack_ser.
*... Estructura interna para la posicion de los seriales de un documento de packing
  DATA: les_packing_ser TYPE  zmm_t_pack_ser.

*... tabla interna para los lotes de un documento de packing
  DATA: lti_packing_lot TYPE STANDARD TABLE OF zmm_t_pack_lot.
*... Estructura interna para la posicion de los lotes de un documento de packing
  DATA: les_packing_lot TYPE  zmm_t_pack_lot.


*  set de atributo packnum
  packnum = i_packnum.
*... Valido que se haya ingresado un numero de documento de packing
  IF i_packnum IS NOT INITIAL.
*.... Consulto la cabecera para los documento de packing
*{   REPLACE        ER3K900332                                        1
*\    SELECT *  FROM zmm_t_packing
*\      INTO TABLE lti_packing
*\      WHERE packnum EQ i_packnum.
    SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD  HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_packing
      INTO TABLE lti_packing
      WHERE packnum EQ i_packnum.
*}   REPLACE
*... valido que hayan encontrado registros de cabecera
    IF lti_packing[] IS NOT INITIAL.
*... cargo en la estructura el registro encontrado de la cabecera del documento de packing
      READ TABLE lti_packing INTO les_packing INDEX 1.
      IF sy-subrc EQ 0.
*... Realizo set del atributo de cabecera del documento de packing
        s_packing = les_packing.
      ENDIF.

*... Consulto el detalle para los documento de packing encontrado
*{   REPLACE        ER3K900332                                        2
*\      SELECT  * FROM zmm_t_packing_po
*\        INTO TABLE lti_packing_po
*\        FOR ALL ENTRIES IN lti_packing
*\        WHERE packnum EQ lti_packing-packnum.
      SELECT  MANDT POSICION PACKNUM POSPACK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT
              UMCC P_CONFIRM DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO UMANIPULACION UMANIPULACION_SE UEMBALAJE
         FROM zmm_t_packing_po
        INTO TABLE lti_packing_po
        FOR ALL ENTRIES IN lti_packing
        WHERE packnum EQ lti_packing-packnum.
*}   REPLACE
*... valido que hayan encontrado registros de detalle del documento de packing
      IF lti_packing_po[] IS NOT INITIAL.
*... Realizo set del atributo de detalle del documento de packing
        t_packing_det = lti_packing_po.
*... Consulto los seriales para los detalles del documento de packing
*{   REPLACE        ER3K900332                                        3
*\        SELECT * FROM zmm_t_pack_ser
*\          INTO TABLE lti_packing_ser
*\          FOR ALL ENTRIES IN lti_packing
*\*          WHERE packnum EQ i_packnum
*\          WHERE     ( vbeln2 EQ lti_packing-vbeln2 AND vbeln2 NE '') OR
*\
*\                   ( aglutinador EQ lti_packing-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ lti_packing-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ lti_packing-rsnum AND rsnum NE '').
        IF les_packing-vbeln2 is not INITIAL.
          SELECT MANDT DOCUMENTO  TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE
           FROM zmm_t_pack_ser
          INTO TABLE lti_packing_ser
          FOR ALL ENTRIES IN lti_packing
*          WHERE packnum EQ i_packnum
          WHERE     ( vbeln2 EQ lti_packing-vbeln2 ).
        ELSEIF les_packing-aufnr is not INITIAL.
          SELECT MANDT DOCUMENTO  TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE
           FROM zmm_t_pack_ser
          INTO TABLE lti_packing_ser
          FOR ALL ENTRIES IN lti_packing
            WHERE  ( aufnr EQ lti_packing-aufnr )  .
        ELSEIF les_packing-rsnum is not INITIAL.
            SELECT MANDT DOCUMENTO  TIPO_DOC MATNR SERIE VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FLAG_SER UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE
           FROM zmm_t_pack_ser
          INTO TABLE lti_packing_ser
          FOR ALL ENTRIES IN lti_packing
            WHERE    ( rsnum EQ lti_packing-rsnum ).
        ENDIF.

*}   REPLACE
        IF sy-subrc EQ 0.
*... Realizo set del atributo de seriales del documento de packing
          t_packing_ser = lti_packing_ser.
        ENDIF.

*... Consulto los lotes para los detalles del documento de packing
*{   REPLACE        ER3K900332                                        4
*\        SELECT * FROM zmm_t_pack_lot
*\          INTO TABLE lti_packing_lot
*\          FOR ALL ENTRIES IN lti_packing
*\*          WHERE packnum EQ i_packnum
*\      WHERE    ( vbeln2 EQ lti_packing-vbeln2 AND vbeln2 NE '') OR
*\
*\                   ( aglutinador EQ lti_packing-aglutinador AND aglutinador NE '') OR
*\                   ( aufnr EQ lti_packing-aufnr AND aufnr NE '' ) OR
*\                   ( rsnum EQ lti_packing-rsnum AND rsnum NE '').

        IF les_packing-vbeln2 is not INITIAL.
          SELECT MANDT DOCUMENTO TIPO_DOC MATNR LOTE UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE CANTIDAD UMC VBELN EBELN AGLUTINADOR VBELN2
               AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
           FROM zmm_t_pack_lot
          INTO TABLE lti_packing_lot
          FOR ALL ENTRIES IN lti_packing
          WHERE     ( vbeln2 EQ lti_packing-vbeln2 ).
        ELSEIF les_packing-aufnr is not INITIAL.
          SELECT MANDT DOCUMENTO TIPO_DOC MATNR LOTE UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE CANTIDAD UMC VBELN EBELN AGLUTINADOR VBELN2
               AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
           FROM zmm_t_pack_lot
          INTO TABLE lti_packing_lot
          FOR ALL ENTRIES IN lti_packing
            WHERE  ( aufnr EQ lti_packing-aufnr )  .
        ELSEIF les_packing-rsnum is not INITIAL.
        SELECT MANDT DOCUMENTO TIPO_DOC MATNR LOTE UBICACION_FI UMANIPULACION UMANIPULACION_SE UEMBALAJE CANTIDAD UMC VBELN EBELN AGLUTINADOR VBELN2
               AUFNR RSNUM CANTIDAD_USO UMC_USO FLAG_LOT
           FROM zmm_t_pack_lot
          INTO TABLE lti_packing_lot
          FOR ALL ENTRIES IN lti_packing
            WHERE    ( rsnum EQ lti_packing-rsnum ).
        ENDIF.

*}   REPLACE
        IF sy-subrc EQ 0.
*... Realizo set del atributo de seriales del documento de packing
          t_packing_lot = lti_packing_lot.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

*  Obtenemos dato de cabecera de transporte.
*{   REPLACE        ER3K900332                                        5
*\  SELECT SINGLE *
*\    FROM zmm_t_transporte
*\    INTO s_packing_tr
*\    WHERE   ( documento EQ les_packing-vbeln2 ) OR
*\            ( documento EQ les_packing-aglutinador AND aglutinador NE '') OR
*\            ( documento EQ les_packing-aufnr ) OR
*\            ( documento EQ les_packing-rsnum ).
  SELECT SINGLE MANDT TKNUM  TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
    FROM zmm_t_transporte
    INTO s_packing_tr
    WHERE   ( documento EQ les_packing-vbeln2 ) OR

            ( documento EQ les_packing-aufnr ) OR
            ( documento EQ les_packing-rsnum ).
*}   REPLACE

  IF sy-subrc EQ 0.
*    Obtener datos de unidad de manipulación.
*{   REPLACE        ER3K900332                                        6
*\    SELECT *
*\      FROM ZMM_T_TRANSP_DT
*\      INTO TABLE t_packing_um
*\      WHERE tknum EQ s_packing_tr-tknum.
    SELECT MANDT TKNUM TKNUM_SE VENUM VENUM_SE EXIDV CONSECUTIVO UBICACION USUARIO FECHA HORA ESTADO
      FROM ZMM_T_TRANSP_DT
      INTO TABLE t_packing_um
      WHERE tknum EQ s_packing_tr-tknum.
*}   REPLACE

  ENDIF.

ENDMETHOD.


METHOD pack_material.

*        Estructura para manejo de posiciones de packing.
  DATA: les_packing_det TYPE zedsd_packing_pos,
*        Tabla manejo de seriales de packing.
        lti_packing_ser TYPE TABLE OF zedsd_packing_ser,
*        Estructura para manejo de serailes de packing.
        les_packing_ser TYPE zedsd_packing_ser,
*        Estructura para manejo de lotes de packing.
        les_packing_lot TYPE zedsd_packing_lot.

*  Manejo de Mensajes de retorno.
  DATA: les_mensajes TYPE zesd_msg_packing.

*  Declaracion de variables.
  DATA: lv_flag TYPE c.

*  Validar que todas posiciones se encuentren en esta OK.
  LOOP AT t_packing_det INTO les_packing_det.
    IF les_packing_det-p_confirm NE 'OK'.
      lv_flag = 'X'.
*    llenar tabla con mensaje de error
      les_mensajes-num_doc = i_tknum.
      les_mensajes-type_msg = 'E'.
*{   REPLACE        ER3K900332                                        2
*\      les_mensajes-msg = 'Existen posiciones con estado diferente a OK'.
      MESSAGE S020(ZCLPACK) INTO les_mensajes-msg.
*}   REPLACE
      APPEND les_mensajes TO c_mensajes.
      EXIT.
    ENDIF.
  ENDLOOP.
*  Si alguna de las posiciones no esta en OK no se embala ninguna.
  CHECK lv_flag IS INITIAL.

*  Recorrer el detalles de packing.
  LOOP AT t_packing_det INTO les_packing_det.
    CLEAR lv_flag.

*    Recorrer lotes del material en caso que tenga.
*    LOOP AT t_packing_lot INTO les_packing_lot
*      WHERE matnr = les_packing_det-material.
*      Embalar material.
*{   REPLACE        ER3K900332                                        1
*\    READ TABLE t_packing_lot INTO les_packing_lot
*\      WITH KEY matnr = les_packing_det-material
*\               umanipulacion = les_packing_det-umanipulacion
*\               cantidad = les_packing_det-cantidad.
    READ TABLE t_packing_lot INTO les_packing_lot
      WITH KEY matnr = les_packing_det-material
               umanipulacion = les_packing_det-umanipulacion.
*               cantidad = les_packing_det-cantidad.
*}   REPLACE
    IF sy-subrc EQ 0.
      CALL METHOD zcl_lgtica_embalaje=>pack_material
        EXPORTING
          i_es_detalle = les_packing_det
          i_es_lote    = les_packing_lot
          i_tknum      = i_tknum
        CHANGING
          c_mensajes   = c_mensajes.
      lv_flag = 'X'.
    ENDIF.
*    ENDLOOP.

    IF lv_flag IS INITIAL.
*      Embalar material.
      CALL METHOD zcl_lgtica_embalaje=>pack_material
        EXPORTING
          i_es_detalle = les_packing_det
          i_tknum      = i_tknum
        CHANGING
          c_mensajes   = c_mensajes.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


  method PACK_MATERIAL_SE.
*{   INSERT         ER3K900332                                        1
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

DATA: lv_vgpos type POSNR.
*        Estructura para manejo de posiciones de packing.
  DATA: les_packing_det TYPE zedsd_packing_pos,
*.....Estructura con Datos Principales de Entrega
          lti_lips TYPE STANDARD TABLE OF ltp_lips,
          les_lips TYPE ltp_lips,
*        Tabla manejo de seriales de packing.
        lti_packing_ser TYPE TABLE OF zedsd_packing_ser,
*        Estructura para manejo de serailes de packing.
        les_packing_ser TYPE zedsd_packing_ser,
*        Estructura para manejo de lotes de packing.
        les_packing_lot TYPE zedsd_packing_lot.

*  Manejo de Mensajes de retorno.
  DATA: les_mensajes TYPE zesd_msg_packing.

*  Declaracion de variables.
  DATA: lv_flag TYPE c.





*  Validar que todas posiciones se encuentren en esta OK.
  LOOP AT t_packing_det INTO les_packing_det.
    IF les_packing_det-p_confirm NE 'OK'.
      lv_flag = 'X'.
*    llenar tabla con mensaje de error
      les_mensajes-num_doc = i_tknum_SE.
      les_mensajes-type_msg = 'E'.
      MESSAGE S020(ZCLPACK) into les_mensajes-msg .
*        les_mensajes-msg = 'Existen posiciones con estado diferente a OK'.
      APPEND les_mensajes TO c_mensajes.
      EXIT.
    ENDIF.
  ENDLOOP.
*  Si alguna de las posiciones no esta en OK no se embala ninguna.
  CHECK lv_flag IS INITIAL.

*.... Se toman las entregas asociadas al Número de documento ingresado
    SELECT vbeln posnr pstyv matnr werks lgort charg lfimg meins vrkme vgbel vgpos lgnum sernr ormng
    INTO TABLE lti_lips
      FROM lips
       WHERE vbeln EQ i_entrega.

*  Recorrer el detalles de packing.
  LOOP AT t_packing_det INTO les_packing_det.
    CLEAR lv_flag.
*.... Válido que la posición pertenezca a la entrega
    READ TABLE lti_lips into les_lips WITH KEY vgpos = les_packing_det-umanipulacion_se(6).
    IF sy-subrc ne 0.
      CONTINUE.
    ENDIF.
*    Recorrer lotes del material en caso que tenga.
*    LOOP AT t_packing_lot INTO les_packing_lot
*      WHERE matnr = les_packing_det-material.
*      Embalar material.
*{   REPLACE        ER3K900332                                        1
*\    READ TABLE t_packing_lot INTO les_packing_lot
*\      WITH KEY matnr = les_packing_det-material
*\               umanipulacion = les_packing_det-umanipulacion
*\               cantidad = les_packing_det-cantidad.
    READ TABLE t_packing_lot INTO les_packing_lot
      WITH KEY matnr = les_packing_det-material
               umanipulacion = les_packing_det-umanipulacion.
*               cantidad = les_packing_det-cantidad.
*}   REPLACE
    IF sy-subrc EQ 0.
*{   REPLACE        ER3K900332                                        3
*\      CALL METHOD zcl_lgtica_embalaje=>pack_material_SE
*\        EXPORTING
*\          i_es_detalle = les_packing_det
*\          i_es_lote    = les_packing_lot
*\          i_tknum      = i_tknum_SE
*\        CHANGING
*\          c_mensajes   = c_mensajes.
      CALL METHOD zcl_lgtica_embalaje=>pack_material_SE
        EXPORTING
          i_es_detalle = les_packing_det
          i_es_lote    = les_packing_lot
          i_tknum      = i_tknum_SE
          i_entrega    = i_entrega
        CHANGING
          c_mensajes   = c_mensajes.
*}   REPLACE
      lv_flag = 'X'.
    ENDIF.
*    ENDLOOP.

    IF lv_flag IS INITIAL.
*      Embalar material.
*{   REPLACE        ER3K900332                                        2
*\      CALL METHOD zcl_lgtica_embalaje=>pack_material_SE
*\        EXPORTING
*\          i_es_detalle = les_packing_det
*\          i_tknum      = i_tknum_SE
*\        CHANGING
*\          c_mensajes   = c_mensajes.
      CALL METHOD zcl_lgtica_embalaje=>pack_material_SE
        EXPORTING
          i_es_detalle = les_packing_det
          i_tknum      = i_tknum_SE
          i_entrega      = i_entrega
        CHANGING
          c_mensajes   = c_mensajes.
*}   REPLACE
    ENDIF.

  ENDLOOP.


*}   INSERT
  endmethod.


METHOD set_create_by_pakble_ref.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Inserta Registros en Tabla de Packing
* Autor Prog.  :
* Fecha Creac. : 10.11.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
*... Cabecera de cabecera documento de packing
  DATA: r_header_packing TYPE zttsd_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_header_packing TYPE zedsd_packing.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_packing.
  DATA: lv_cantser TYPE i.
  DATA: lv_cantmat TYPE i.
*.....Variable de Ref a la Clase Picknum
  DATA:   lo_lgtica_packing TYPE REF TO zcl_lgtica_packing.
*... tabla interna para la cabecera del documento de packing
  DATA: lti_packing TYPE STANDARD TABLE OF zmm_t_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_packing TYPE zmm_t_packing.
*... tabla interna para la posicion del documento de packing
  DATA: lti_packing_dt TYPE STANDARD TABLE OF zmm_t_packing_po.
*... Tabla para agrupar datos de detalle
  DATA: lti_packing_dt_ag TYPE STANDARD TABLE OF zmm_t_packing_po.
*... tabla interna para la posicion del documento de packing tabla Z
  DATA: lti_zmm_t_packing_dt TYPE STANDARD TABLE OF zmm_t_packing_po.

*... Estructura para posición del documento de packing tabla Z
  DATA: les_packing_dt TYPE zmm_t_packing_po.
*... Estructura para posición del documento de packing tabla Z
  DATA: les_packing_dt_aux TYPE zmm_t_packing_po.
*... Cabecera de posicion documento de packing
  DATA: r_pos_packing TYPE zttsd_packing_pos.
*... Estructura para posición del documento de packing
  DATA: les_pos_packing TYPE zedsd_packing_pos.
*... Estructura para posición de seriales documento de packing
  DATA: les_packing_ser TYPE zedsd_packing_ser.
*... Estructura para posición de lotes documento de picking
  DATA: les_packing_lot TYPE zedsd_packing_lot.
*.... Cantidad de material y seriales.
*  DATA: lv_cantser TYPE i.
*  DATA: lv_cantmat TYPE i.

  DATA: lv_consecutivo TYPE zed_pospick.

  "Declarar variables
  DATA: wobjeto      TYPE tnro-object    VALUE 'ZEDPACKING', " Nombre del SNRO
        wnorange     TYPE inri-nrrangenr VALUE '01',        "Número de rango,
        wsubobj      TYPE inri-subobject VALUE space,       "Subobject
        w_doc_number TYPE char10.

  DATA:  p_doc_number TYPE char18. " Variable para almacenar el número generado

*... Llamo el metodo para Retornara la Estructura de Cabecera para un document de packing
  CALL METHOD i_ref_packable->get_header_packing
    RECEIVING
      r_header_packing = r_header_packing.

*... Válido que existan registros
  IF r_header_packing[] IS NOT INITIAL.

*... Consulto todos los registros que se hayan creado con anterioridad
*{   REPLACE        ER3K900332                                        4
*\    SELECT *
*\      FROM zmm_t_packing
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_packing
      SELECT MANDT PACKNUM PROCESO SUBPROCESO VBELN EBELN AGLUTINADOR VBELN2 AUFNR RSNUM FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
      FROM zmm_t_packing
      INTO  TABLE lti_packing
*}   REPLACE
      FOR ALL ENTRIES IN  r_header_packing
      WHERE vbeln2 EQ r_header_packing-vbeln2
      AND vbeln EQ r_header_packing-vbeln
      AND ebeln EQ r_header_packing-ebeln
      AND aglutinador EQ r_header_packing-aglutinador
      AND aufnr EQ r_header_packing-aufnr
      AND rsnum EQ r_header_packing-rsnum.

*... Llamo el metodo para Retornara la Estructura de detalle para un document de packing
    CALL METHOD i_ref_packable->get_pos_packing
      RECEIVING
        r_pos_packing = r_pos_packing.

    IF lti_packing IS NOT INITIAL.
     READ TABLE lti_packing into les_packing INDEX 1.
*... Elimino los lotes y seriales del documento de packing
*{   REPLACE        ER3K900332                                        3
*\     DELETE from zmm_t_pack_ser where VBELN2 = les_packing-VBELN2 .
*\     DELETE FROM zmm_t_pack_lot where VBELN2 = les_packing-VBELN2 .
     IF les_header_packing-vbeln2 is not INITIAL.
       DELETE from zmm_t_pack_ser where VBELN2 = les_header_packing-vbeln2 .
       DELETE FROM zmm_t_pack_lot where VBELN2 = les_header_packing-vbeln2 .
     ELSEIF les_header_packing-aufnr is not INITIAL.
       DELETE from zmm_t_pack_ser where aufnr = les_header_packing-aufnr .
       DELETE FROM zmm_t_pack_lot where aufnr = les_header_packing-aufnr .
     ELSEIF les_header_packing-rsnum is not INITIAL.
       DELETE from zmm_t_pack_ser where rsnum = les_header_packing-rsnum .
       DELETE FROM zmm_t_pack_lot where rsnum = les_header_packing-rsnum .
     ENDIF.

*}   REPLACE
*... Consulto las posiciones de packing
*{   REPLACE        ER3K900332                                        5
*\      SELECT *
*\      INTO  CORRESPONDING FIELDS OF TABLE lti_zmm_t_packing_dt
      SELECT MANDT POSICION PACKNUM POSPACK MATERIAL CANTIDAD UMC DIFERENCIA UMD UBICACION_TMP UBICACION_SU UBICACION_FI LOTE FECHA HORA CONTEO CANTCONT
             UMCC P_CONFIRM  DOC_ASOCIADO VBELN2 RSNUM AUFNR USUARIO UMANIPULACION UMANIPULACION_SE UEMBALAJE
      INTO  TABLE lti_zmm_t_packing_dt
*}   REPLACE
      FROM zmm_t_packing_po
      FOR ALL ENTRIES IN lti_packing
      WHERE  packnum EQ lti_packing-packnum.
*... Ordeno la tabla de posiciones para el documento de packing
      SORT lti_zmm_t_packing_dt BY pospack DESCENDING.
*... Leo el primer registro para obtener el consecutivo
      READ TABLE lti_zmm_t_packing_dt INTO les_packing_dt INDEX 1.
      IF sy-subrc EQ 0.
        lv_consecutivo = les_packing_dt-pospack.
      ENDIF.

**...  Elimino los registros que son del usuario que rezaliza el set
*     DELETE lti_zmm_t_packing_dt where ( usuario eq i_usuario  or usuario is initial ).
*      DELETE FROM zmm_t_packing_po where ( usuario eq i_usuario  or usuario EQ '' ).

*... Ordeno tabla por posición  material y ubición
     SORT    lti_zmm_t_packing_dt by posicion material UBICACION_TMP descending.
     lti_packing_dt = lti_zmm_t_packing_dt.
     DELETE ADJACENT DUPLICATES FROM  lti_packing_dt comparing posicion material UBICACION_TMP .
*... Agrupo las cantidades
     LOOP AT lti_packing_dt INTO les_packing_dt.
*         lti_packing_dt = lti_packing_dt_ag .
         CLEAR: les_packing_dt-cantidad ,les_packing_dt-diferencia , les_packing_dt-cantcont.
         LOOP AT lti_zmm_t_packing_dt into les_pos_packing where posicion = les_packing_dt-posicion and  material = les_packing_dt-material
           and material = les_packing_dt-material.
         les_packing_dt-cantidad = les_packing_dt-cantidad + les_pos_packing-cantidad.
         les_packing_dt-diferencia = les_packing_dt-diferencia + les_pos_packing-diferencia.
         les_packing_dt-cantcont = les_packing_dt-cantcont + les_pos_packing-cantcont.
         ENDLOOP.
         APPEND les_packing_dt TO lti_packing_dt_ag.
     ENDLOOP.

     lti_zmm_t_packing_dt = lti_packing_dt_ag.
*... Recorro los registros consultados de la tabla Z
      LOOP AT lti_zmm_t_packing_dt INTO les_packing_dt.

        clear:lti_packing_dt.
*... Guardo la cantidad contada del material
        lv_cantmat = les_packing_dt-cantcont.
*... Elimino todas las posiciones de la tabla Z
        delete from zmm_t_packing_po where packnum = les_packing_dt-packnum and
        posicion = les_packing_dt-posicion.
*... Recorro los registros originales y los agrego
        LOOP AT r_pos_packing INTO les_pos_packing WHERE vbeln2 = les_packing_dt-vbeln2 AND rsnum = les_packing_dt-rsnum AND aufnr = les_packing_dt-aufnr
          AND posicion = les_packing_dt-posicion AND ubicacion_tmp EQ les_packing_dt-ubicacion_tmp.

          lv_consecutivo = lv_consecutivo + 1.
          les_pos_packing-packnum = les_packing_dt-packnum.
          les_pos_packing-pospack = lv_consecutivo.
          les_pos_packing-diferencia = les_pos_packing-cantidad - les_pos_packing-cantcont.
**          IF i_usuario ne les_packing_dt-usuario.
**            les_pos_packing-diferencia = les_pos_packing-diferencia - les_packing_dt-cantcont.
**            les_pos_packing-cantidad = les_pos_packing-cantidad - les_packing_dt-cantcont.
**          ENDIF.

          CLEAR:les_packing_dt_aux.
          READ TABLE lti_packing_dt INTO les_packing_dt_aux WITH KEY packnum = les_packing_dt-packnum posicion = les_pos_packing-posicion.
          IF les_packing_dt_aux IS INITIAL.
*            IF les_pos_packing-cantidad NE 0.
              APPEND les_pos_packing TO lti_packing_dt.
*            ENDIF.
          ENDIF.

        ENDLOOP.

        MODIFY zmm_t_packing_po FROM TABLE lti_packing_dt.
      ENDLOOP.


    ENDIF.

  ENDIF.


ENDMETHOD.


METHOD update_packing_by_cant.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo que Distribuye Registros en Tabla de posiciones packing
* Autor Prog.  :
* Fecha Creac. : 15.07.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
  DATA: le_flg_aglutinador TYPE c.
*... Cabecera de cabecera documento de packing
  DATA: r_header_packing TYPE zttsd_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_header_packing TYPE zedsd_packing.
*... Tablas para el manejo de EANS
  DATA: t_detalle TYPE STANDARD TABLE OF  zesd_eanmat,
        t_mensajes TYPE STANDARD TABLE OF zesd_msg_packing.
*... Estructuras para el manejo de ENAS
  DATA: les_detalle TYPE zesd_eanmat.
*... tabla interna para la cabecera del documento de packing
  DATA: lti_packing TYPE STANDARD TABLE OF zmm_t_packing.
*... Estructura para la cabecera del documento de packing
  DATA: les_packing TYPE zmm_t_packing.
*... tabla interna para la posicion del documento de packing
  DATA: lti_packing_dt TYPE STANDARD TABLE OF zmm_t_packing_po.
*... Estructura para posición del documento de packing
  DATA: les_packing_dt TYPE zedsd_packing_pos.
*... Estructura para el material
  DATA: l_e_matnr TYPE matnr.
*... Código de barras
  DATA: l_e_cod_bar TYPE zed_cod_bar.

  DATA: les_cab_posiciones TYPE zedsd_packing_pos.
*... Cabecera de posicion documento de packing
  DATA: r_pos_packing TYPE zttsd_packing_pos.
*... Estructura para posición del documento de packing
  DATA: les_reg_posiciones TYPE zedsd_packing_pos.

*.... Posición en el loop
  DATA: lv_index TYPE sy-tabix.

*.... Cantidad diferencia total
  DATA: lv_cantdif_tot TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcontcab_umb TYPE zedsd_cant_cnt.

*.... Cantidad contada en el dispositivo para un material UMB
  DATA: lv_cantcont_umb TYPE zedsd_cant_cnt.
*.... Cantidad contada en el dispositio para un material UMD
  DATA: lv_cantcont_umd TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositivo para un material UMB
  DATA: lv_cantdif_umb TYPE zedsd_cant_cnt.
*.... Cantidad diferencia en el dispositio para un material UMD
  DATA: lv_cantdif_umd TYPE zedsd_cant_cnt.

  DATA: lv_consecutivo TYPE zed_pospack VALUE 1000.

**... Campos Errores en Procesamiento Cabecera
  DATA: les_msg TYPE zesd_msg_packing.
*.....Variable Texto Error
  DATA:          l_e_desc_error TYPE string.

*.....Variable Texto cantidad excedida Error
  DATA:          l_e_cexc_error TYPE string.
*.... Tablas auxiliares para guardar la información
  DATA: lti_cab_reg_posiciones TYPE zttsd_packing_pos,
        lti_reg_posiciones TYPE zttsd_packing_pos.

*... Guardo en las tablas auxiliares los valores por defecto
  lti_reg_posiciones  = c_reg_posiciones.
  lti_cab_reg_posiciones = c_cab_posiciones.


  DATA: lv_p_confirm TYPE zed_flag_confirm.
*.... Adición de lotes
  CALL METHOD zcl_lgtica_packing=>add_ser_lot
    EXPORTING
      i_documento_origen = i_documento_origen
      i_entrega          = i_entrega
      i_tipo_documento   = i_tipo_documento
      i_clase_documento  = i_clase_documento
      i_usuario          = i_usuario
      c_mensajes         = c_mensajes
    CHANGING
      c_packing_ser      = c_packing_ser
      c_packing_lot      = c_packing_lot.

  SORT c_reg_posiciones BY p_confirm ASCENDING.

  LOOP AT c_reg_posiciones INTO les_reg_posiciones WHERE p_confirm NE 'A'.
    CLEAR:les_packing_dt ,lv_cantdif_tot,lv_cantcontcab_umb,l_e_cod_bar.

*{   REPLACE        ER3K900339                                        5
*\*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
*\    l_e_cod_bar = les_reg_posiciones-umanipulacion.
*\    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
*\      EXPORTING
*\        i_rotulo = l_e_cod_bar
*\      IMPORTING
*\        e_exidv  = les_reg_posiciones-umanipulacion.
IF les_reg_posiciones-umanipulacion is not INITIAL.
*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
    l_e_cod_bar = les_reg_posiciones-umanipulacion.
    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo
      EXPORTING
        i_rotulo = l_e_cod_bar
      IMPORTING
        e_exidv  = les_reg_posiciones-umanipulacion.
ENDIF.

IF les_reg_posiciones-umanipulacion_se is not INITIAL.
*... cambio unidad de manipulación leída por el dispositivo a Umanipulación iterna
    l_e_cod_bar = les_reg_posiciones-umanipulacion_se.
    CALL METHOD zcl_lgtica_embalaje=>get_um_rotulo_se
      EXPORTING
        i_rotulo = l_e_cod_bar
      IMPORTING
        e_exidv  = les_reg_posiciones-umanipulacion.

        les_reg_posiciones-umanipulacion_se = les_reg_posiciones-umanipulacion.
        clear les_reg_posiciones-umanipulacion.

ENDIF.
*}   REPLACE

*... consulto el registro de cabecera del dispositivo
    READ TABLE c_cab_posiciones INTO les_cab_posiciones WITH KEY material = les_reg_posiciones-material.

*... Copio la información de la tabla de posiciones del documento de packing a una tabla auxiliar
    lti_packing_dt = c_packing_det_sup.

*... filtro solo los materiales
    DELETE lti_packing_dt WHERE material NE les_reg_posiciones-material.
*... Es un Aglutinador
*{   DELETE         ER3K900279                                       12
*\    IF les_reg_posiciones-aufnr IS NOT INITIAL.
*\      DELETE lti_packing_dt WHERE aufnr NE les_reg_posiciones-aufnr.
*\      le_flg_aglutinador = 'X'.
*\    ENDIF.
*}   DELETE
*... Ordeno la tabla de posiciones para el documento de packing
    SORT c_packing_det_sup BY pospack DESCENDING.
*... Leo el primer registro para obtener el consecutivo
    READ TABLE c_packing_det_sup INTO les_packing_dt INDEX 1.
    IF sy-subrc EQ 0.
      lv_consecutivo = les_packing_dt-pospack.
    ENDIF.

*...filtro los que tengan diferencia o cantidad para contar disponible
    DELETE lti_packing_dt WHERE diferencia = 0 AND p_confirm = 'OK' OR p_confirm = 'CT'.

    CLEAR:t_detalle.
*... Consulto los EANS de el material
    CALL FUNCTION 'Z_SD_CONSUL_EAN_MATE'
       EXPORTING
         matnr            = les_reg_posiciones-material
        TABLES
*       T_MARA           =
          t_detalle        = t_detalle
          t_mensajes       = t_mensajes
                .
*... Acumulo la cantidad que puedo contar
    LOOP AT lti_packing_dt INTO les_packing_dt.
      lv_cantdif_tot = lv_cantdif_tot + les_packing_dt-diferencia.
    ENDLOOP.
*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
    IF les_packing_dt-rsnum IS NOT INITIAL .
*... Si tiene reserva se toma la unidad de medida de la posición de packing
      READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_packing_dt-umc.
    ELSE.
*... Si no tiene reserva se toma la unidad de medida de la cabecera del dispositivo
      READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_cab_posiciones-umcc.
    ENDIF.

    IF sy-subrc EQ 0.
      lv_cantcontcab_umb = les_cab_posiciones-cantcont /  les_detalle-cantidad_ean.
    ENDIF.
    les_reg_posiciones-p_confirm = ''.
*... Valido si se conto mas de lo que puedo contar
    IF lv_cantcontcab_umb <= lv_cantdif_tot.

      IF lv_cantcontcab_umb < lv_cantdif_tot.
        lv_p_confirm = 'NE'.
      ELSEIF  lv_cantcontcab_umb = lv_cantdif_tot.
        lv_p_confirm = 'OK'.
      ENDIF.

*... Guardo la cantidad contada del dispositivo
      lv_cantcont_umb = les_reg_posiciones-cantcont.
      lv_cantcont_umd = lv_cantcont_umb.

*... Convierto la cantidad total contada enviada del dispositivo y la convierto a UMD
      IF les_packing_dt-rsnum IS NOT INITIAL .
*... Si tiene reserva se toma la unidad de medida de la posición de packing
        READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_packing_dt-umc.
      ELSE.
*... Si no tiene reserva se toma la unidad de medida de la cabecera del dispositivo
        READ TABLE t_detalle INTO les_detalle WITH KEY unidad_medida = les_reg_posiciones-umc.
      ENDIF.
      IF sy-subrc EQ 0.
        lv_cantcont_umd = lv_cantcont_umb /  les_detalle-cantidad_ean.
      ENDIF.

**... verificamos que al menos haya un registro con las condiciones
*    IF les_packing_dt is not initial.
      IF lv_cantcont_umd > 0 .
*... Recorro las posiciones del documento de packing con Unidad temporal  y Fija establecidas
*{   REPLACE        ER3K900339                                        6
*\        LOOP AT lti_packing_dt  INTO les_packing_dt
*\        WHERE material = les_reg_posiciones-material  AND "posicion = les_reg_posiciones-posicion AND "vbeln2 = les_reg_posiciones-vbeln2 AND
*\        diferencia > 0  AND ubicacion_tmp = les_reg_posiciones-ubicacion_tmp AND uembalaje EQ les_reg_posiciones-uembalaje AND umanipulacion EQ les_reg_posiciones-umanipulacion.
        LOOP AT lti_packing_dt  INTO les_packing_dt
        WHERE material = les_reg_posiciones-material  AND "posicion = les_reg_posiciones-posicion AND "vbeln2 = les_reg_posiciones-vbeln2 AND
        diferencia > 0  AND ubicacion_tmp = les_reg_posiciones-ubicacion_tmp AND uembalaje EQ les_reg_posiciones-uembalaje AND umanipulacion EQ les_reg_posiciones-umanipulacion.
*}   REPLACE

*... Si la cantidad contada es menor que la cantidad disponible para la posicion del documento de packing asigno toda la cantidad contada
          IF lv_cantcont_umd <  les_packing_dt-diferencia AND lv_cantcont_umd > 0 .
            les_packing_dt-cantcont = les_packing_dt-cantcont + lv_cantcont_umd.
            les_packing_dt-umcc  = les_packing_dt-umc.
            les_packing_dt-diferencia = les_packing_dt-cantidad - les_packing_dt-cantcont.
            les_packing_dt-umd  = les_packing_dt-umc.
            les_packing_dt-conteo = les_reg_posiciones-conteo.
            IF les_reg_posiciones-usuario IS INITIAL.
              les_packing_dt-usuario = i_usuario.
            ELSE.
              les_packing_dt-usuario = les_reg_posiciones-usuario.
            ENDIF.
*          les_packing_dt-p_confirm = 'OK'.
            les_packing_dt-p_confirm = lv_p_confirm.
            lv_cantcont_umd = 0.
            MODIFY lti_packing_dt FROM les_packing_dt TRANSPORTING cantcont diferencia p_confirm conteo usuario WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack ."INDEX sy-tabix.
*{   REPLACE        ER3K900279                                        3
*\            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umd.
*}   REPLACE
            les_cab_posiciones-umcc  = les_packing_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
*          MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.
            EXIT.
          ELSEIF lv_cantcont_umd > 0.
*... Si la cantidad contada es mayor o igual que la cantidad disponible para la posicion del documento de packing asigno lo que falta (Diferencia )
*... y Actualizo la cantidad contada
            lv_cantcont_umd = lv_cantcont_umd - les_packing_dt-diferencia.
            les_packing_dt-cantcont =  les_packing_dt-cantcont + les_packing_dt-diferencia.
            les_packing_dt-umcc  = les_packing_dt-umc.
            les_packing_dt-diferencia = les_packing_dt-cantidad - les_packing_dt-cantcont.
            les_packing_dt-umd  = les_packing_dt-umc.
            les_packing_dt-conteo = les_reg_posiciones-conteo.
            IF les_reg_posiciones-usuario IS INITIAL.
              les_packing_dt-usuario = i_usuario.
            ELSE.
              les_packing_dt-usuario = les_reg_posiciones-usuario.
            ENDIF.
*            les_packing_dt-usuario = i_usuario.
*          les_packing_dt-p_confirm = 'OK'.
            les_packing_dt-p_confirm = lv_p_confirm.
            MODIFY lti_packing_dt FROM les_packing_dt TRANSPORTING cantcont diferencia umcc umd p_confirm conteo usuario WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack ."INDEX sy-tabix.

            MODIFY c_packing_det_sup FROM les_packing_dt TRANSPORTING cantcont diferencia  umcc umd p_confirm usuario WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack ."INDEX sy-tabix.

*{   REPLACE        ER3K900279                                        2
*\            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - les_packing_dt-cantcont  .
*}   REPLACE
            les_cab_posiciones-umcc  = les_packing_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
          ENDIF.

        ENDLOOP.
      ENDIF.
*Si la cantidad contada es mayor a 0 , se necesita crear otro registro
      IF lv_cantcont_umd > 0 .
*... Recorro las posiciones del documento de packing con sin unidad temporal y fija
*{   REPLACE        ER3K900339                                        7
*\        LOOP AT lti_packing_dt  INTO les_packing_dt
*\        WHERE material = les_reg_posiciones-material  " AND posicion = les_reg_posiciones-posicion "vbeln2 = les_reg_posiciones-vbeln2 AND
*\          AND diferencia > 0 AND ubicacion_tmp = les_reg_posiciones-ubicacion_tmp AND uembalaje IS INITIAL AND ( umanipulacion IS INITIAL OR umanipulacion EQ '0000000').
        LOOP AT lti_packing_dt  INTO les_packing_dt
        WHERE material = les_reg_posiciones-material  " AND posicion = les_reg_posiciones-posicion "vbeln2 = les_reg_posiciones-vbeln2 AND
          AND diferencia > 0 AND ubicacion_tmp = les_reg_posiciones-ubicacion_tmp AND uembalaje IS INITIAL AND ( umanipulacion IS INITIAL OR umanipulacion EQ '0000000')
          .
*}   REPLACE

*... Si la cantidad contada es menor que la cantidad disponible para la posicion del documento de packing
*... actualizo el registro quitandole la cantidad contada y creo otro registro con toda la cantidad contada
          IF lv_cantcont_umd <  les_packing_dt-diferencia AND lv_cantcont_umd > 0.

            les_packing_dt-cantidad = les_packing_dt-cantidad - lv_cantcont_umd.
            les_packing_dt-umcc  = les_packing_dt-umc.
            les_packing_dt-diferencia = les_packing_dt-cantidad - les_packing_dt-cantcont.
            les_packing_dt-umd  = les_packing_dt-umc.
            les_packing_dt-conteo = les_reg_posiciones-conteo.
            MODIFY lti_packing_dt FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont umd umcc p_confirm conteo WHERE packnum = les_packing_dt-packnum AND
          pospack = les_packing_dt-pospack .
*            MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.

            MODIFY c_packing_det_sup FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont umd umcc WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack ."INDEX sy-tabix.

            lv_consecutivo = lv_consecutivo + 1.
            les_packing_dt-pospack = lv_consecutivo.
            les_packing_dt-cantidad = lv_cantcont_umd.
            les_packing_dt-cantcont = lv_cantcont_umd.
            les_packing_dt-diferencia = les_packing_dt-cantidad - les_packing_dt-cantcont.
            les_packing_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
            les_packing_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
            les_packing_dt-uembalaje = les_reg_posiciones-uembalaje .
            les_packing_dt-umanipulacion = les_reg_posiciones-umanipulacion .
*            les_packing_dt-usuario = i_usuario.
*{   INSERT         ER3K900279                                        9
*            les_packing_dt-umanipulacion_se = les_reg_posiciones-umanipulacion_se .
*}   INSERT

            IF les_reg_posiciones-usuario IS INITIAL.
              les_packing_dt-usuario = i_usuario.
            ELSE.
              les_packing_dt-usuario = les_reg_posiciones-usuario.
            ENDIF.

*            les_packing_dt-p_confirm = 'OK'.
            les_packing_dt-p_confirm = lv_p_confirm.
            APPEND les_packing_dt TO lti_packing_dt.
            APPEND les_packing_dt TO c_packing_det_sup.

*{   REPLACE        ER3K900279                                        4
*\            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umd.
*}   REPLACE
            les_cab_posiciones-umcc  = les_packing_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
            lv_cantcont_umd = 0.
            EXIT.
          ELSEIF lv_cantcont_umd > 0.
*... Si la cantidad contada es mayor o igual que la cantidad disponible para la posicion del documento de packing asigno lo que falta (Diferencia )
*... y Actualizo la cantidad contada
*            les_packing_dt-cantidad =  les_packing_dt-cantidad - les_packing_dt-diferencia.
*            MODIFY lti_packing_dt from les_packing_dt index sy-tabix.
            lv_cantcont_umd = lv_cantcont_umd -  les_packing_dt-diferencia.
            les_packing_dt-cantcont =  les_packing_dt-cantcont + les_packing_dt-diferencia.
            les_packing_dt-umcc  = les_packing_dt-umc.
            les_packing_dt-diferencia = les_packing_dt-cantidad - les_packing_dt-cantcont.
            les_packing_dt-umd  = les_packing_dt-umc.
            les_packing_dt-ubicacion_tmp =  les_reg_posiciones-ubicacion_tmp .
            les_packing_dt-ubicacion_fi = les_reg_posiciones-ubicacion_fi .
            les_packing_dt-umanipulacion = les_reg_posiciones-umanipulacion .
            les_packing_dt-uembalaje = les_reg_posiciones-uembalaje .
*{   INSERT         ER3K900339                                        8
*            les_packing_dt-umanipulacion_se = les_reg_posiciones-umanipulacion_se .
*}   INSERT
            les_packing_dt-usuario = i_usuario.
            les_packing_dt-conteo = les_reg_posiciones-conteo.
            IF les_reg_posiciones-usuario IS INITIAL.
              les_packing_dt-usuario = i_usuario.
            ELSE.
              les_packing_dt-usuario = les_reg_posiciones-usuario.
            ENDIF.
*            les_packing_dt-usuario = i_usuario.
*            les_packing_dt-p_confirm = 'OK'.
            les_packing_dt-p_confirm = lv_p_confirm.
*{   REPLACE        ER3K900279                                       10
*\            MODIFY lti_packing_dt FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi umd umcc p_confirm conteo usuario uembalaje umanipulacion WHERE packnum = les_packing_dt-packnum AND
*\            pospack = les_packing_dt-pospack .
            MODIFY lti_packing_dt FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi umd umcc p_confirm conteo usuario uembalaje umanipulacion umanipulacion_se WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack .
*}   REPLACE
*            MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.

*{   REPLACE        ER3K900279                                       11
*\            MODIFY c_packing_det_sup FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi  umd umcc p_confirm usuario uembalaje umanipulacion WHERE packnum = les_packing_dt-packnum AND
*\            pospack = les_packing_dt-pospack .
            MODIFY c_packing_det_sup FROM les_packing_dt TRANSPORTING cantidad diferencia cantcont ubicacion_tmp ubicacion_fi  umd umcc p_confirm usuario uembalaje umanipulacion umanipulacion WHERE packnum = les_packing_dt-packnum AND
            pospack = les_packing_dt-pospack .
*}   REPLACE

*{   REPLACE        ER3K900279                                        1
*\            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - lv_cantcont_umb.
            les_cab_posiciones-cantcont = les_cab_posiciones-cantcont - les_packing_dt-cantcont.
*}   REPLACE
            les_cab_posiciones-umcc  = les_packing_dt-umc.
            MODIFY c_cab_posiciones FROM les_cab_posiciones TRANSPORTING cantcont umcc WHERE material = les_cab_posiciones-material .
*            APPEND les_packing_dt  to lti_packing_dt.
          ENDIF.

        ENDLOOP.

      ENDIF.

      IF lv_cantcont_umd = 0.
        LOOP AT lti_packing_dt INTO les_packing_dt.
          les_packing_dt-p_confirm = lv_p_confirm.
          MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.
        ENDLOOP.
        MODIFY  zmm_t_packing_po FROM TABLE lti_packing_dt.
*... Guardo en las tablas auxiliares los valores por defecto
        lti_reg_posiciones  = c_reg_posiciones.
        lti_cab_reg_posiciones = c_cab_posiciones.
      ELSE.
        LOOP AT lti_packing_dt INTO les_packing_dt.
          les_packing_dt-p_confirm = 'NM'.
          MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.

          READ TABLE lti_reg_posiciones INTO les_reg_posiciones WITH KEY packnum = les_packing_dt-packnum
          material = les_packing_dt-material
          posicion = les_packing_dt-posicion.
          IF sy-subrc EQ 0 .
            les_reg_posiciones-p_confirm = 'NM'.
            MODIFY lti_reg_posiciones FROM les_reg_posiciones INDEX sy-tabix.
          ENDIF.

        ENDLOOP.
*        MODIFY  zmm_t_packing_dt FROM TABLE lti_packing_dt.
      ENDIF.
    ELSE.
*      IF le_flg_aglutinador is initial.
*    ....  Añado la novedad a cada registro del material
      LOOP AT lti_packing_dt INTO les_packing_dt.

        les_packing_dt-p_confirm = 'NM'.
        MODIFY lti_packing_dt FROM les_packing_dt INDEX sy-tabix.

        READ TABLE lti_reg_posiciones INTO les_reg_posiciones WITH KEY packnum = les_packing_dt-packnum
        material = les_packing_dt-material
        posicion = les_packing_dt-posicion.
        IF sy-subrc EQ 0 .
          les_reg_posiciones-p_confirm = 'NM'.
          MODIFY lti_reg_posiciones FROM les_reg_posiciones INDEX sy-tabix.
        ENDIF.

      ENDLOOP.
*    ....   Modifico la tabla Z con los NM
      MODIFY  zmm_t_packing_po FROM TABLE lti_packing_dt.
*    .... Cantidad contada excedida
      l_e_cexc_error = lv_cantcontcab_umb - lv_cantdif_tot.

*    .... Guardo Mensaje
      les_msg-num_doc = i_documento_origen.
*{   REPLACE        ER3K900279                                       13
*\      CONCATENATE 'Material ' les_packing_dt-material 'Excede cantidad en ' l_e_cexc_error
*\      INTO l_e_desc_error SEPARATED BY space.
*\      les_msg-msg = l_e_desc_error.
      MESSAGE S026(ZCLPACK) WITH les_packing_dt-material l_e_cexc_error into les_msg-msg.
*}   REPLACE
      les_msg-type_msg = 'E'.
      APPEND les_msg TO c_mensajes.

*      ELSE.
**.... Si es un aglutinador y se realiza una adicion
*          lv_p_confirm = 'OK'.
*      ENDIF.

    ENDIF.
  ENDLOOP.
*.... Recorro los registros que son adiciones
  LOOP AT c_reg_posiciones INTO les_reg_posiciones WHERE p_confirm EQ 'A'.
    lv_p_confirm = 'OK'.
    CLEAR:les_packing_dt ,lv_cantdif_tot,lv_cantcontcab_umb.
*... Copio la información de la tabla de posiciones del documento de packing a una tabla auxiliar
    lti_packing_dt = c_packing_det_sup.
*... consulto el registro de cabecera del dispositivo
    READ TABLE c_cab_posiciones INTO les_cab_posiciones WITH KEY material = les_reg_posiciones-material.

  ENDLOOP.


ENDMETHOD.
ENDCLASS.
