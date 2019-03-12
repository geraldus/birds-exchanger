import React from 'react'
import FinancialReportView from './financial-report'

export default () => {
        const root = document.querySelector('#' + window.app.config.su.rootId)
        const wsAddr = window.app.config.su.socket
        const lbls = window.app.config.su.labels
        ReactDOM.render(
            <FinancialReportView
                socket={wsAddr}
                labels={lbls}
            />,
            root)
}