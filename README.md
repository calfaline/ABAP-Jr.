# ABAP-Jr.

*&---------------------------------------------------------------------*
*& Report  ZALINE_REV27
*&
*&---------------------------------------------------------------------*
REPORT zaline_rev27.
*&---------------------------------------------------------------------*
*                       ---ESTRUTURA VBAK---
*&---------------------------------------------------------------------*
TYPES: BEGIN OF est_vbak,
         vbeln TYPE vbak-vbeln,
         erdat TYPE vbak-erdat,
         ernam TYPE vbak-ernam,
         auart TYPE vbak-auart,
         desc  TYPE char30,
         vbtyp TYPE vbak-vbtyp,
         bezei TYPE tvakt-bezei,
         netwr TYPE vbak-netwr,
         waerk TYPE vbak-waerk,
         texto TYPE char30,
       END OF est_vbak.
DATA: wa_est TYPE est_vbak.
DATA: v_text TYPE char50.

*&---------------------------------------------------------------------*
*                       ---ESTRUTURA TVAKT---
*&---------------------------------------------------------------------*
TYPES: BEGIN OF est_tvakt,
         bezei TYPE tvakt-bezei,
         spras TYPE tvakt-spras,
         auart TYPE tvakt-auart,
       END OF est_tvakt.
*&---------------------------------------------------------------------*
*                       ---TABELA INTERNA---
*&---------------------------------------------------------------------*
DATA: it_est TYPE TABLE OF est_vbak.
DATA: csv_converted_table TYPE truxs_t_text_data.

DATA: it_est_tvakt TYPE TABLE OF est_tvakt.
DATA: wa_est_tvakt TYPE est_tvakt.
*&---------------------------------------------------------------------*
*                         ---VARIÁVEIS---
*&---------------------------------------------------------------------*
  DATA: lv_window_title TYPE string,
        lv_sel_folder   TYPE string,
        download TYPE char30.
*&---------------------------------------------------------------------*
*                           ---FILTRO---
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK dados.
SELECT-OPTIONS: p_vbeln FOR wa_est-vbeln DEFAULT 7101 TO 7102 NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK dados.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-l00.
PARAMETERS: p_down    AS CHECKBOX .

PARAMETERS p_local   TYPE string DEFAULT 'C:\Users\Aline.Calfa\Desktop\Arquivo csv'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
  p_down = 'X'.
*&---------------------------------------------------------------------*
*                     ---AT SELECTION SCREEN---
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_local.
*CALL METHOD CL_GUI_FRONTEND_SERVICES=>directory_browse.

 if p_down = 'X'.
        "Arquivo Local
        CLEAR : lv_sel_folder.
        CALL METHOD cl_gui_frontend_services=>directory_browse
          EXPORTING
            window_title         = lv_window_title
          CHANGING
            selected_folder      = lv_sel_folder
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        IF sy-subrc = 0.
          p_local = lv_sel_folder.
        ENDIF.
ENDIF.
*&---------------------------------------------------------------------*
*                     ---INICIO DO PROGRAMA---
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select.
  PERFORM description.
  PERFORM convert_csv.
  PERFORM download.
*&---------------------------------------------------------------------*
*&      Form  SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select.
  SELECT vbeln
       erdat
       ernam
       auart
       vbtyp
       netwr
       waerk
    FROM vbak INTO CORRESPONDING FIELDS OF TABLE it_est
      WHERE vbeln IN p_vbeln.

  SELECT bezei
         spras
         auart
    FROM tvakt INTO TABLE it_est_tvakt
      FOR ALL ENTRIES IN it_est
        WHERE auart = it_est-auart AND spras = sy-langu.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_CSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_csv.
  CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
    EXPORTING
      i_field_seperator    = ';'
    TABLES
      i_tab_sap_data       = it_est
    CHANGING
      i_tab_converted_data = csv_converted_table.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
*  download = p_local.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = p_local
      filetype = 'ASC'
    TABLES
      data_tab = csv_converted_table.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM description.

  LOOP AT it_est INTO wa_est .

    CALL FUNCTION 'TB_DOMAINVALUE_GET_TEXT'
      EXPORTING
        name  = 'VBTYP'
        value = wa_est-vbtyp
        langu = sy-langu
      IMPORTING
        text  = v_text.
    wa_est-desc = v_text.

    LOOP AT it_est_tvakt INTO wa_est_tvakt WHERE auart = wa_est-auart .
      wa_est-bezei = wa_est_tvakt-bezei.
    ENDLOOP.

    IF wa_est-netwr > 10000.

      wa_est-texto = text-001.

    ENDIF.

    MODIFY it_est FROM wa_est.

  ENDLOOP.
ENDFORM.
