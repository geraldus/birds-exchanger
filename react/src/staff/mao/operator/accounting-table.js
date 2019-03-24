import React from 'react'
import _ from 'lodash'
import moment from 'moment'

import History from './accounting-table/history'
import Totals from './accounting-table/totals'
import Form from './accounting-table/form'


export default class MaoOperatorAccountingTable extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            isSignedIn: false,
            data: {
                sheets: [],
                selected: null,
                history: [],
                totals: [],
                investments: []
            },
            formVisible: false,
            formState: '',
            anyUpdates: false
        }
        this.sheetId = '1gl5eHg2AFGKrQnJM_WUWs1qKk9UyACSGEIUZ3M-0o4M'
        this.authorizeButton =
            <button className="btn btn-outline" onClick={this.handleAuthClick}>Вход</button>
        this.signoutButton =
            <button className="btn btn-outline mr-2" onClick={this.handleSignoutClick}>Выход</button>
        this.depositButton =
            <button className="btn btn-outline mx-2" onClick={_ => this.handleDepositButton()}>Пополнение</button>
        this.withdrawalButton =
            <button className="btn btn-outline mx-2" onClick={() => this.handleWithdrawalButton()}>Вывод</button>
        this.form = React.createRef()
        // bindings
        this.initGoogleClient = this.initGoogleClient.bind(this)
        this.updateSigninStatus = this.updateSigninStatus.bind(this)
        this.fetchSheetsData = this.fetchSheetsData.bind(this)
        this.handleTabSelect = this.handleTabSelect.bind(this)
    }
    componentDidMount () {
        const self = this
        $(document).ready(() => {
            gapi.load('client:auth2', self.initGoogleClient)
        })
    }
    initGoogleClient () {
        const update = this.updateSigninStatus
        const self = this
        gapi.client.init({
            apiKey: this.props.apiKey,
            clientId: this.props.clientId,
            discoveryDocs: this.props.discoveryDocs,
            scope: this.props.scopes
        }).then(function () {
            // Listen for sign-in state changes.
            const auth = gapi.auth2.getAuthInstance()
            self.updateSigninStatus(auth.isSignedIn.get())
            auth.isSignedIn.listen(update)
        }, function(error) {})
    }
    updateSigninStatus(isSignedIn) {
        const self = this
        if (isSignedIn) {
            gapi.client.sheets.spreadsheets.get({
                spreadsheetId: self.sheetId
            }).then(r => {
                // console.log(r.result)
                let sdata = r.result.sheets
                self.fetchSheetsData(sdata)
                self.setState(s => _.merge({}, s, { data: { sheets: sdata } }))
            })
        }
        this.setState({isSignedIn: isSignedIn})
    }
    fetchSheetsData (sheets) {
        const self = this
        let r1 = sheets[0].properties.title + '!A2:J'
        let r2 = sheets[1].properties.title + '!A2:J'
        let r3 = sheets[2].properties.title + '!A2:J'
        let sid = this.sheetId
        gapi.client.sheets.spreadsheets.values.get({
            spreadsheetId: sid,
            range: r1
        }).then(r => {
            self.setState(s =>
                _.merge( {}, s, { data: {
                    history: r.result.values.map(v => ({ updated: false, value: v })),
                    selected: s.data.selected === null? 0 : s.data.selected
                } }))
        })
        gapi.client.sheets.spreadsheets.values.get({
            spreadsheetId: sid,
            range: r2
        }).then(r => {
            self.setState(s =>
                _.merge( {}, s, { data: { totals: r.result.values.map(v => ({ updated: false, value: v })) } }))
        })
    }
    handleAuthClick () {
        gapi.auth2.getAuthInstance().signIn()
    }
    handleSignoutClick () {
        gapi.auth2.getAuthInstance().signOut();
    }
    handleTabSelect (tabId) {
        const self = this
        return (e) => {
            self.setState(s => _.merge({}, s, { data: { selected: tabId }}))
            e.preventDefault()
        }
    }
    handleDepositButton () {
        this.setState({
            formVisible: true,
            formState: 'd'
        })
    }
    handleWithdrawalButton () {
        this.setState({
            formVisible: true,
            formState: 'w'
        })
    }
    handleFormDismiss () {
        this.setState({
            formVisible: false,
            formState: ''
        })
    }
    getPersonList () {
        const { history, totals } = this.state.data
        let p1 = [], p2 = []
        if (history && history.length > 0) {
            p1 = [ ... new Set(history.map(v => v.value[1])) ]
        }
        if (totals && totals.length > 0) {
            p2 = [ ... new Set(totals.map(v => v.value[0])) ]
        }
        return [ ... new Set([ ... p1, ... p2]) ]
    }
    updateSheets (formData) {
        const formatN = x => {
            let parts = x.toString().split('.');
            parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ' ')
            return parts.join('.')
        }
        const parse = x => (parseFloat(x.replace(/[^\d\.]/g, '')) || 0)
        const { currency, person } = formData
        const amount = parseFloat(formData.amount)
        const ratio = parseFloat(formData.ratio)
        console.log('FD', amount, ratio, person, currency)
        // 1. add history record
        const date = moment().format('DD.MM.YY')
        let d = [ date, person, '', '', '', '', '', '', '' ]
        if (currency === 'rur') {
            if (amount > 0) {
                d[2] = formatN(amount)
                d[3] = ratio
            } else {
                d[4] = formatN(amount)
                d[5] = ratio
            }
        } else {
            if (amount > 0) {
                d[6] = amount
            } else {
                d[7] = -amount
            }
        }
        this.setState(s => _.merge({}, s, {
            data: {
                history: [ ... s.data.history, { updated: true, value: d } ]
            }
        }))
        // 2. update totals
        const { data: { totals } } = this.state
        let recId = totals.findIndex(e => e.value[0] == person)
        let coins = currency === 'pzm'? amount : amount / ratio
        let x, y, tid, cin = 0, cout = 0, ts, t = [ person, '0', '0', '0', '', '' ]
        if (recId !== -1) {
            t = totals[recId].value
            cin = parse(totals[recId].value[1])
            cout = parse(totals[recId].value[2])
        } else {
            recId = totals.findIndex(e => e.value[0] == '')
            if (recId === -1) recId = totals.length
        }
        if (coins > 0) {
            tid = 1
            x = cin + coins
            y = x - cout
        } else {
            tid = 2
            x = cout + Math.abs(coins)
            y = cin - x
        }
        t[tid] = formatN(x)
        t[3] = formatN(y)
        ts = totals
        ts[recId] = {
            updated: true,
            value: t
        }
        this.setState(s => _.merge({}, s, {
            data: {
                totals: ts
            },
            anyUpdates: true
        }))
        this.handleFormDismiss()
    }
    saveSheet () {
        const date = moment().format('DD.MM.YY_x')
        const self = this
        const { data: { sheets, history, totals } } = this.state
        gapi.client.sheets.spreadsheets.create({
            properties: {
              title: 'Обновление_' + date
            },
            sheets: sheets.map(s => ({ properties: s.properties }))
        }).then((response) => {
            const sheetId = response.result.spreadsheetId
            const r1 = sheets[0].properties.title + '!A1'
            const r2 = sheets[1].properties.title + '!A1'
            const v1 = history.map(x => x.value)
            const v2 = totals.map(x => x.value)
            let data = [
                {
                    range: r1,
                    values: v1
                },
                {
                    range: r2,
                    values: v2
                }
            ]
            const body = {
                data: data,
                valueInputOption: 'RAW'
            }
            gapi.client.sheets.spreadsheets.values.batchUpdate({
                spreadsheetId: sheetId,
                resource: body
            }).then((response) => {
                var result = response.result;
                console.log(`${result.totalUpdatedCells} cells updated.`);
                self.setState({ anyUpdates: false })
            })
        });
    }
    render () {
        const { isSignedIn, anyUpdates, data: { sheets, history, totals, selected } } = this.state
        const personList = this.getPersonList()
        return (<React.Fragment>
            <div className="row mb-2">
                {!isSignedIn && this.authorizeButton}
                {isSignedIn  && <React.Fragment>
                    {this.signoutButton}
                    {this.depositButton}
                    {this.withdrawalButton}
                    {anyUpdates && <button className="btn btn-danger mx-2" onClick={_ => this.saveSheet()}>Сохранить</button>}
                </React.Fragment>}
            </div>
            {isSignedIn && sheets.length > 0 && <React.Fragment>
                <div className="row mb-2">
                    <ul className="nav nav-tabs">
                        {sheets.map((v, i) =>
                            <li className="nav-item">
                                <a
                                    className={`nav-link${selected == i? ' active' : ''}`}
                                    href="#"
                                    onClick={this.handleTabSelect(i)}>
                                    {v.properties.title}
                                </a>
                            </li>
                        )}
                    </ul>
                </div>
                {this.state.formVisible && <div className="container-fluid my-1">
                    <Form
                        ref={f => this.form = f}
                        persons={personList}
                        onSubmit={d => this.updateSheets(d)}
                        onDismiss={_ => this.handleFormDismiss()}
                        operation={this.state.formState}/>
                </div>}
                {history.length > 0 && selected == 0 && <div className="row">
                    <History values={history}/>
                </div>}
                {totals.length > 0 && selected == 1 && <div className="row">
                    <Totals values={totals}/>
                </div>}
            </React.Fragment>}
        </React.Fragment>)
    }
}