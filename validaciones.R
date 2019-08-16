load.lib('dplyr', 'stringr')



bd_8750 <- read.table('C:/Users/c804324/Downloads/VWJRL0910201708750_ATM.csv', sep = ';', header = FALSE, fill = T)

nombres <- c("ACF_Tipo_Cuenta", "ACF_Monto_en_Moneda_Local", "ACF_Monto_Original_TRX", "ACF_Hora_TRX", "ACF_Fecha_TRX", "ACF_Entry_Mode", "ACF_Autorizacion", "ACF_Cod_Rpta", "ACF_ID_Terminal_Direccion_IP", "ACF_ID_Comercio", "ACF_Canal", "ACF_Bin", "ACF_Cod_Pais_Adq", "ACF_Cod_Pais_Emisor", "ACF_ID_Emisor", "ACF_Cod_Moneda_Trx", "ACF_Cod_Moneda_TH", "ACF_Tipo_MSJ", "ACF_Cod_Trx", "ACF_Monto_Dollar", "ACF_Cod_Banco_Destino_ORG_Destino", "ACF_Fecha_Aplicacion", "ACF_Codigo_CIO_Agencia_Oficina_Origen", "ACF_Pais_Origen_&_MCC", "ACF_IP_Region", "ACF_IP_Nombre_de_Pais", "ACF_IP_Nombre_Ciudad", "ACF_IP_Codigo_Pais_2_Digitos", "ACF_IP_Codigo_de_Pais_3_Numeros", "Tarjeta_Virtual", "ACF_Localidad_Comercio", "ACF_Nombre_Localizacion_Comercio", "ACF_Tarjeta_registro_750", "ACF_Tipo_ID_Cliente", "ACF_ID_Cliente")

names(bd_8750) <- nombres


bd_8750$ACF_Fecha_TRX[1]


filtro <- bd_8750 %>% filter(ACF_Monto_en_Moneda_Local == '9000000000,00')

table(bd_8750$ACF_Fecha_TRX)


montos <- as.numeric(str_replace_all(bd_8750$ACF_Monto_en_Moneda_Local, ',', '.'))
summary(montos)



