import React from 'react'
import { FinancialReportView } from "./staff/super-user/financial-report"

$(document).ready(() => {
    const root = document.querySelector('#react-host')
    const wsAddr = window.app.config.su.socket
    ReactDOM.render(<FinancialReportView socket={wsAddr}/>, root)
})