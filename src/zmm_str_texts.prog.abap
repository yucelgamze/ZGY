*&---------------------------------------------------------------------*
*& Report ZMM_STR_TEXTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_str_texts.

DATA:lv_content_tolerans       TYPE string,
     lv_content_pr_approval    TYPE string,
     lv_content_po_approval    TYPE string,
     lv_content_po_approval_pr TYPE string.

" so10 - ZMM_000_STR_TOLERANS
lv_content_tolerans =
'<!DOCTYPE html>' &&
'<html lang="en">' &&
'<head>' &&
'<meta charset="utf-8">' &&
'</head>' &&
'<body style="background-color:#FFFFFF;">' &&
'<p>Sayın İlgili,</p>' &&
'<p>' &&
'<EBELN>' &&
' nolu satınalma siparişinde' &&
' teslimat toleransı değiştirilmiştir.' &&
'</p>' &&
'<p>Belgeyi lütfen kontrol ediniz.</p><br>' &&
'<p>SAS Detayları</p>' &&
'<table border=1>' &&
'<tr><th bgcolor = "#E6E6FA" align = "LEFT" >SAS No</th>' &&
'<th  bgcolor = "#E6E6FA" align = "LEFT" >Kalem No</th></tr>' &&
'<tr><td>' &&
'<EBELN>' &&
'</td>' &&
'<td>' &&
'<EBELP>' &&
'</td></tr>' &&
'</table>' &&
'</body>' &&
'</html>'.

" so10 - ZMM_032_STR_PR_APPROVAL
lv_content_pr_approval =
'<!DOCTYPE html>' &&
'<html>' &&
'<head>' &&
'<meta charset="utf-8">' &&
'</head>' &&
'<body style="background-color:#FFFFFF;">' &&
'<p>Sayın İlgili,</p>' &&
'<p>' &&
'<BANFN>' &&
' numaralı SAT kalemi/kalemleri onaylanmıştır. </p>' &&
'<p>Belge detaylarını kontrol edebilirsiniz.</p>' &&
'<p>İyi çalışmalar.</p>' &&
'<table>' &&
'<tr>' &&
'<td><b> SAT Onay Tarihi</b></td>' &&
'<td>' &&
'<FRGDT>' &&
'</td></tr>' &&
'<tr>' &&
'<td><b> Satınalma Sorumlusu</b></td>' &&
'<td>' &&
'<SAGS>' &&
'</td></tr>' &&
'</table> <br> <br>' &&
'<p>SAT Detayları</p>' &&
'<table border=1>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme No </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Adı </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Miktar </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Ölçü Birimi </th>' &&
'</tr>' &&
'<tr><td>' &&
'<MATNR>' &&
'</td>' &&
'<td>' &&
'<MAKTX>' &&
'</td>' &&
'<td>' &&
'<MENGE>' &&
'</td>' &&
'<td>' &&
'<MEINS>' &&
'</td></tr>' &&
'</table>' &&
'</body>' &&
'</html>'.


" so10 - ZMM_033_STR_PO_APPROVAL
lv_content_po_approval =
'<!DOCTYPE html>' &&
'<html>' &&
'<head>' &&
'<meta charset="utf-8">' &&
'</head>' &&
'<body style="background-color:#FFFFFF;">' &&
'<p>Sayın İlgili,</p>' &&
'<p>' &&
'Aşağıdaki' &&
' SAS kalemi/kalemleri onaylanmıştır. Sipariş teyit formu ektedir.</p>' &&
'<p>Belge detaylarını kontrol edebilirsiniz.</p>' &&
'<table border=1>' &&
'<tr>' &&
'<td bgcolor = "#E6E6FA" align = "LEFT"><b> Satınalma Siparişi</b></td>' &&
'<td bgcolor = "#FFFFFF" align = "LEFT">' &&
'<EBELN>' &&
'</td></tr>' &&
'<tr>' &&
'<td bgcolor = "#E6E6FA" align = "LEFT"><b> Tedarikçi Firma</b></td>' &&
'<td bgcolor = "#FFFFFF" align = "LEFT">' &&
'<NAME1>' &&
'</td></tr>' &&
'</table>' &&
'</body>' &&
'</html>'.


" so10 - ZMM_033_STR_PO_APPROVAL_PR
lv_content_po_approval_pr =
'<!DOCTYPE html>' &&
'<html>' &&
'<head>' &&
'<meta charset="utf-8">' &&
'</head>' &&
'<body style="background-color:#FFFFFF;">' &&
'<p>Sayın İlgili,</p>' &&
'<p>Satınalma talebinize istinaden' &&
' aşağıdaki' &&
' SAS kalemi/kalemleri' &&
' onaylanmıştır.</p>' &&
'<p>Belge detaylarını kontrol edebilirsiniz.</p>' &&
'<p>İyi çalışmalar dileriz.</p>' &&
'<p>Satınalma Departmanı</p>' &&
'<table>' &&
'<tr>' &&
'<td><b> SAT No</b></td>' &&
'<td>' &&
'<BANFN>' &&
'</td></tr>' &&
'<tr>' &&
'<td><b> SAS No</b></td>' &&
'<td>' &&
'<EBELN>' &&
'</td></tr>' &&
'<tr>' &&
'<td><b> Siparişi Oluşturan</b></td>' &&
'<td>' &&
'<ERNAM>' &&
'</td></tr>' &&
'<tr>' &&
'<td><b> SAS Onay Tarihi</b></td>' &&
'<td>' &&
'<DATUMX>' &&
'</td></tr>' &&
'<tr>' &&
'<td><b> Tedarikçi Firma</b></td>' &&
'<td>' &&
'<NAME1>' &&
'</td></tr>' &&
'</table> <br> <br>' &&
'<p>SAS Detayları</p>' &&
'<table border=1>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Kodu </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Tanımı </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Miktar </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Ölçü Birimi </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Depo Yeri </th>' &&
'<th bgcolor = "#E6E6FA" align = "LEFT" > Teslimat Tarihi </th>' &&
'</tr>' &&
'<tr><td>' &&
'<MATNR>' &&
'</td>' &&
'<td>' &&
'<MAKTX>' &&
'</td>' &&
'<td>' &&
'<MENGE>' &&
'</td>' &&
'<td>' &&
'<MEINS>' &&
'</td>' &&
'<td>' &&
'<LGOBE>' &&
'</td>' &&
'<td>' &&
'<EINDT>' &&
'</td></tr>' &&
'</table>' &&
'</body>' &&
'</html>'.

BREAK gamzey.
