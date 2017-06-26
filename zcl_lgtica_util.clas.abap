class ZCL_LGTICA_UTIL definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_UTIL
*"* do not include other source files here!!!
public section.

  class-methods GET_DETALLES_USUARIO
    importing
      value(I_USUARIO) type ZED_USUARIO_MOVIL optional
    exporting
      value(E_DET_USUARIO) type ZMM_T_IMP_ETIQ
    changing
      value(C_MENSAJES) type ZTTSD_MSG_PICKING optional .
  class-methods AJUSTAR_DOCUMENTO
    importing
      !I_DOCUMENTO type ZE_DOCUMENTO
    returning
      value(R_DOCUMENTO) type ZE_DOCUMENTO .
  class-methods MODIFY_ZMM_T_TRANSPORTE
    importing
      !I_TI_TRANSPORTE type ZMM_TT_TRANSPORTE
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
  class-methods MODIFY_ZMM_T_TRANSP_DT
    importing
      !I_TI_TRANSP_DT type ZMM_TT_TRANSP_DT
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
  class-methods GET_CONSECUTIVO
    importing
      !I_TKNUM type TKNUM optional
      !I_TKNUM_SE type ZED_TKNUM optional
    exporting
      !E_CONSECUTIVO type ZED_CONSECUTIVO .
  class-methods MODIFY_ZMM_T_PICKING_DT
    importing
      !I_TI_PICKING_DT type ZTTSD_PICKING_DET_SUP
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
  class-methods MODIFY_ZMM_T_PCK_SER
    importing
      !I_TI_PICK_SER type ZTTSD_PICKING_SER
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
  class-methods MODIFY_ZMM_T_PCK_LOT
    importing
      !I_TI_PICK_LOT type ZTTSD_PICKING_LOT
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
  class-methods MODIFY_ZMM_T_PICKING
    importing
      !I_ES_PICKING type ZEDSD_PICKING
      !I_MOD type ZED_MOD
    exporting
      !E_DBCNT like SY-DBCNT
      !E_SUBRC like SY-SUBRC .
protected section.
*"* protected components of class ZCL_LGTICA_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_UTIL IMPLEMENTATION.


method AJUSTAR_DOCUMENTO.
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
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = i_documento
    importing
      output = r_documento.
endmethod.


METHOD get_consecutivo.
*{   REPLACE        ER3K900332                                        1
*\
*\  SELECT MAX( consecutivo )
*\    FROM zmm_t_transp_dt
*\    INTO e_consecutivo
*\    WHERE tknum EQ i_tknum
*\    and tknum_se eq i_tknum_se.
DATA: lti_transporte TYPE TABLE OF zmm_t_transporte,
      les_transporte TYPE zmm_t_transporte.

IF i_tknum is INITIAL.
    SELECT SINGLE  MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
        FROM zmm_t_transporte
        INTO les_transporte
        WHERE tknum_se eq i_tknum_se.
ELSE.
    SELECT SINGLE  MANDT TKNUM TKNUM_SE DOCUMENTO TIPO_DOC  VBELN AGLUTINADOR FECHA HORA USUARIO UNAME FEMOD HOMOD USRMOD UNAMOD ESTADO
        FROM zmm_t_transporte
        INTO les_transporte
    WHERE tknum EQ i_tknum.
ENDIF.
IF les_transporte is not INITIAL.
  SELECT MAX( consecutivo )
    FROM zmm_t_transp_dt
    INTO e_consecutivo
    WHERE tknum EQ les_transporte-tknum
    and tknum_se eq les_transporte-tknum_se.

ENDIF.

*}   REPLACE

  IF sy-subrc EQ 0.
    e_consecutivo = e_consecutivo + 1.
  ELSE.
    e_consecutivo = 1.
  ENDIF.


ENDMETHOD.


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
  call function 'Z_SD_CONSUL_DAT_USER_MOV'
    exporting
      usuario     = i_usuario
    importing
      usuario_det = e_det_usuario
    tables
      t_mensajes  = c_mensajes.
endmethod.


method MODIFY_ZMM_T_PCK_LOT.
  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_pck_lot FROM TABLE I_TI_PICK_LOT.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_pck_lot FROM TABLE I_TI_PICK_LOT.
    WHEN 'D'. " Borrar
      DELETE zmm_t_pck_lot FROM TABLE I_TI_PICK_LOT.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_pck_lot FROM TABLE I_TI_PICK_LOT.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
COMMIT WORK AND WAIT.
endmethod.


method MODIFY_ZMM_T_PCK_SER.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_pck_ser FROM TABLE I_TI_PICK_SER.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_pck_ser FROM TABLE I_TI_PICK_SER.
    WHEN 'D'. " Borrar
      DELETE zmm_t_pck_ser FROM TABLE I_TI_PICK_SER.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_pck_ser FROM TABLE I_TI_PICK_SER.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.

COMMIT WORK AND WAIT.
endmethod.


method MODIFY_ZMM_T_PICKING.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_picking FROM  I_es_PICKING.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_picking FROM I_es_PICKING.
    WHEN 'D'. " Borrar
      DELETE zmm_t_picking FROM I_es_PICKING.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_picking FROM  I_es_PICKING.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
COMMIT work  AND WAIT.
endmethod.


method MODIFY_ZMM_T_PICKING_DT.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_picking_dt FROM TABLE I_TI_PICKING_DT.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_picking_dt FROM TABLE I_TI_PICKING_DT.
    WHEN 'D'. " Borrar
      DELETE zmm_t_picking_dt FROM TABLE I_TI_PICKING_DT.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_picking_dt FROM TABLE I_TI_PICKING_DT.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
COMMIT work  AND WAIT.
endmethod.


method MODIFY_ZMM_T_TRANSPORTE.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_transporte FROM TABLE i_ti_transporte.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_transporte FROM TABLE i_ti_transporte.
    WHEN 'D'. " Borrar
      DELETE zmm_t_transporte FROM TABLE i_ti_transporte.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_transporte FROM TABLE i_ti_transporte.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.

commit WORK AND WAIT.
endmethod.


METHOD modify_zmm_t_transp_dt.

  CASE i_mod.
    WHEN 'I'. " Insertar
      INSERT zmm_t_transp_dt FROM TABLE i_ti_transp_dt.
    WHEN 'M'. " Modificar
      MODIFY zmm_t_transp_dt FROM TABLE i_ti_transp_dt.
    WHEN 'D'. " Borrar
      DELETE zmm_t_transp_dt FROM TABLE i_ti_transp_dt.
    WHEN 'U'. " Actualizar
      UPDATE zmm_t_transp_dt FROM TABLE i_ti_transp_dt.
  ENDCASE.

*  Asigna codigo de retorno.
  e_subrc = sy-subrc.
*  Lineas tratadas en tabla.
  e_dbcnt = sy-dbcnt.
  commit WORK AND WAIT.
ENDMETHOD.
ENDCLASS.
