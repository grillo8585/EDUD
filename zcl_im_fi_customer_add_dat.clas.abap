class ZCL_IM_FI_CUSTOMER_ADD_DAT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_CUSTOMER_ADD_DAT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA .
protected section.
*"* protected components of class ZCL_IM_FI_CUSTOMER_ADD_DAT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_CUSTOMER_ADD_DAT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_CUSTOMER_ADD_DAT IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ACCOUNT_NUMBER.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ADD_ON_ACTIVE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ALL_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_DATA_CHANGED.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~GET_CHANGEDOCS_FOR_OWN_TABLES.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~INITIALIZE_ADD_ON_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~MODIFY_ACCOUNT_NUMBER.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~PRESET_VALUES_CCODE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~PRESET_VALUES_SAREA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~READ_ADD_ON_DATA.
endmethod.


METHOD if_ex_customer_add_data~save_data.
  DATA: no_badi     TYPE i,
        v_kunnr     TYPE kunnr.

  IMPORT v_kunnr = v_kunnr
         no_badi = no_badi
    FROM MEMORY ID 'ID'.
  IF no_badi = 1.
    v_kunnr = i_kunnr.
    CLEAR: no_badi.
    EXPORT v_kunnr = v_kunnr
           no_badi = no_badi
        TO MEMORY ID 'ID'.
  ENDIF.
ENDMETHOD.


method IF_EX_CUSTOMER_ADD_DATA~SET_USER_INPUTS.
endmethod.
ENDCLASS.
