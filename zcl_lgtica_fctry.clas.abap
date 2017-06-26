class ZCL_LGTICA_FCTRY definition
  public
  final
  create public .

*"* public components of class ZCL_LGTICA_FCTRY
*"* do not include other source files here!!!
public section.

  constants PEDIDOS_VENTAS type ZE_TIPO_DOC value 'PVTA'. "#EC NOTEXT
  constants TRASLADOS_MERCANCIA type ZE_TIPO_DOC value 'TRAS'. "#EC NOTEXT
  constants ORDENES_PROYECTOS type ZE_TIPO_DOC value 'PROY'. "#EC NOTEXT
  constants SUBCONTRATACIONES type ZE_TIPO_DOC value 'SUBC'. "#EC NOTEXT
  constants DEV_PROVEEDORES type ZE_TIPO_DOC value 'DEVO'. "#EC NOTEXT
  constants INTERCOMPANY type ZE_TIPO_DOC value 'ICOM'. "#EC NOTEXT
  constants ORD_SER_INTERNAS type ZE_TIPO_DOC value 'OSIN'. "#EC NOTEXT
  constants ORD_PROD_SIN_AGLUTINADOR type ZE_TIPO_DOC value 'OPSG'. "#EC NOTEXT
  constants RESERVA_INVT type ZE_TIPO_DOC value 'RINV'. "#EC NOTEXT

  class-methods GET_TIPO_DOC
    importing
      value(I_TIPO_DOC) type ZE_TIPO_DOC
      value(I_USUARIO) type ZED_USUARIO_MOVIL
    returning
      value(R_LGTICA) type ref to ZIF_LGTICA_DOCUMENTO
    raising
      CX_SY_REF_IS_INITIAL .
protected section.
*"* protected components of class ZCL_LGTICA_FCTRY
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LGTICA_FCTRY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LGTICA_FCTRY IMPLEMENTATION.


METHOD get_tipo_doc.
*-------------------------------------------------------------------------------*
* Información General
*-------------------------------------------------------------------------------*
* Descripción  : Metodo Encargado de Instanciar Objeto Según el Tipo de Documento
* Autor Prog.  : Marco Suarez
* Fecha Creac. : 26.06.2014
*-------------------------------------------------------------------------------*
* Ordenes de Transporte
*-------------------------------------------------------------------------------*
* Fecha       | CR#         | Autor           | Modificación
*-------------------------------------------------------------------------------*
* 26.06.2014    ER6K906849    Marco Suarez        Creación
*-------------------------------------------------------------------------------*

*.....Variables de Ref para las SubClases
  DATA :
*.....Variable de Ref para Pedidos de Ventas
        lo_pedidos_venta TYPE REF TO zcl_lgtica_pedidos_venta,
*.....Variable de Ref para Ordenes de Servicio en Proyectos
        lo_proyectos TYPE REF TO zcl_lgtica_proyectos,
*.....Variable de Ref para Ordenes de Servicio Externas
        lo_ord_ser_externas TYPE REF TO ZCL_LGTICA_ORD_SERV_EXT,
*.....Variable de Ref para Traslados de Mercancia
        lo_traslados TYPE REF TO zcl_lgtica_traslados,
*.....Variable de Ref para Intercompany
        lo_intercompany TYPE REF TO zcl_lgtica_intercompany,
*.....Variable de Ref para pedidos subcontratados
        lo_pedidos_subc TYPE REF TO zcl_lgtica_pedidos_subc,
*.....Variable de Ref para devolucion proveedores
        lo_devolucion_prov TYPE REF TO zcl_lgtica_devolucion_prov,
*.....Variable de Ref para Ordenes de Servicio Interna
        lo_ord_ser_internas TYPE REF TO zcl_lgtica_ord_serv_internas,
*.....Variable de Ref para Ordenes de Produccion sin Aglutinador
        lo_ord_prod_sin_aglut TYPE REF TO zcl_lgtica_ord_prod_sin_aglut,
*.....Variable de Ref para Ordenes de Produccion sin Aglutinador
        lo_ord_prod_con_aglut TYPE REF TO zcl_lgtica_ord_prod_con_aglut,
*.....Variable de Ref para Reserva de Inventario
        lo_reserva_inventario TYPE REF TO zcl_lgtica_reserva_inv ,
*.....Variable de Ref para Entregas
        lo_entregas TYPE REF TO zcl_lgtica_entregas.

  CASE i_tipo_doc.
*.....Pedidos de ventas
    WHEN 'PVTA'.
      CREATE OBJECT lo_pedidos_venta
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_pedidos_venta.
      RETURN.

*.....Traslados de mercancía
    WHEN 'TRAS'.
      CREATE OBJECT lo_traslados
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_traslados.
      RETURN.

*.....Ordenes de servicios en proyectos
    WHEN 'PROY'.
      CREATE OBJECT lo_proyectos
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_proyectos.
      RETURN.

*.....Subcontrataciones
    WHEN 'SUBC'.
      CREATE OBJECT lo_pedidos_subc
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_pedidos_subc.

*.....Devoluciones a proveedores
    WHEN 'DEVO'.
      CREATE OBJECT lo_devolucion_prov
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_devolucion_prov.
*.....Intercompany
    WHEN 'ICOM'.
      CREATE OBJECT lo_intercompany
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica   = lo_intercompany.
      RETURN.
*.....Ord.Serv Internas
    WHEN 'OSIN'.
      CREATE OBJECT lo_ord_ser_internas
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_ord_ser_internas.

*.....Ord.Serv Internas
    WHEN 'OSEN'.
      CREATE OBJECT lo_ord_ser_externas
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_ord_ser_externas.
*.....Ordenes de Servicio sin Aglutinador
    WHEN 'OPSG'.
      CREATE OBJECT lo_ord_prod_sin_aglut
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_ord_prod_sin_aglut.

*.....Ordenes de Servicio sin Aglutinador
    WHEN 'OPAG'.
      CREATE OBJECT lo_ord_prod_con_aglut
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_ord_prod_con_aglut.
*.....Reserva de Inventario
    WHEN 'RINV'.
      CREATE OBJECT lo_reserva_inventario
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_reserva_inventario.
*.....Reserva de Inventario
    WHEN 'ENTR'.
      CREATE OBJECT lo_entregas
        EXPORTING
          i_tipo_doc = i_tipo_doc
          i_usuario  = i_usuario.

      r_lgtica = lo_entregas.

*.....El Proceso no esta Vigente
    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_sy_ref_is_initial.
  ENDCASE.
ENDMETHOD.
ENDCLASS.
