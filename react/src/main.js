import React from 'react'
import { FinancialReportView } from "./staff/super-user/financial-report"

const root = document.querySelector('#react-host')
$(document).ready(() => {
    console.log('ready')
    ReactDOM.render(<FinancialReportView/>, root)
})