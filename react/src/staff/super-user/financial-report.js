import React from 'react'
import _ from 'lodash'

export class FinancialReportView extends React.Component {
    constructor(props) {
        const defLabels = {
            userCount: "User count",
            innerProfit: "Inner Profit",
            activeDeposits: "Active Deposits",
            acceptedDeposits: "Accepted Deposits",
            fee: {
                stats: "Fee Stats"
            },
            deposit: {
                stats: "Deposit Stats",
                income: {
                    realTotal: "Deposited Money",
                    feeTotal: "Fee Collected"
                }
            },
            withdrawal: {
                stats: "Withdrawal Stats",
                new: {
                    count: "Awaiting Execution",
                    amount: "Withdrawal Amount Total",
                    frozen: "Amount Frozen"
                },
                accepted: {
                    count: "Executed",
                    transfered: "Amount Transfered",
                    fee: "Fee Collected"
                }
            },
            wallets: {
                stats: "Wallet Stats",
                balanceTotals: "Wallet Balances Total"
            }
        }
        super(props)
        this.props.labels = _.merge(defLabels, props.labels)
        // Don't call this.setState() here!
        this.state = {
            userCount: 0,
            innerProfit: {},
            activeDeposit: {
                count: 0,
                items: []
            },
            acceptedDeposit: {
                count: 0,
                items: []
            },
            deposit: {
                income: {
                    real: {},
                    fee: {}
                }
            },
            withdrawal: {
                new: {
                    count: 0,
                    amountStats: {},
                    frozenStats: {}
                },
                accepted: {
                    count: 0,
                    feeStats: {},
                    transferStats: {}
                }
            },
            wallets: {
                total: {}
            }
        }
        this.webSocket = new WebSocket(props.socket)
        this.webSocket.onopen = this.webScoketOnOpen.bind(this)
        this.webSocket.onmessage = this.webScoketOnMessage.bind(this)
        this.handleJsonMessage = this.handleJsonMessage.bind(this)
        this.handleCountEvent = this.handleCountEvent.bind(this)
    }

    webScoketOnOpen () {
        this.webSocket.send('user count')
        this.webSocket.send('inner profit stats')
        this.webSocket.send('active deposit count')
        this.webSocket.send('accepted deposit count')
        this.webSocket.send('deposited money')
        this.webSocket.send('wallet stats')
        this.webSocket.send('withdrawal stats')
    }

    webScoketOnMessage (e) {
        try {
            const j = JSON.parse(e.data)
            this.handleJsonMessage(j)
        } catch (error) {
            console.log('Socket message', e.data)
        }
    }

    handleJsonMessage (json) {
        switch (json.type) {
            case 'count-event':
                this.handleCountEvent(json.object, json.value)
                break
            default:
                console.log('Unexpected type', json)
        }
    }

    handleCountEvent (obj, val) {
        const s = this.state
        switch (obj) {
            case 'User Count':
                this.setState({
                    userCount: val
                })
                break
            case 'Active Deposit Count':
                this.setState({
                    activeDeposit: { count: val }
                })
                break
            case 'Accepted Deposit Count':
                this.setState({
                    acceptedDeposit: { count: val }
                })
                break
            case 'Inner Profit':
                this.setState({
                    innerProfit: val
                })
                break
            case 'Deposited Money':
                this.setState(_.merge(
                    {}, s, { deposit: { income: { real: val } } }
                ))
                break
            case 'Deposit Fee':
                this.setState(_.merge(
                    {}, s, { deposit: { income: { fee: val } } }
                ))
                break
            case 'Wallet Stats':
                this.setState({
                    wallets: {
                        total: val
                    }
                })
                break
            case 'Withdrawal New Count':
                this.setState(_.merge({}, s, { withdrawal: { new: { count: val } } }))
                break
            case 'Withdrawal Accepted Count':
                this.setState(_.merge({}, s, { withdrawal: { accepted: { count: val } } }))
                break
            case 'Withdrawal New Amount Stats':
                this.setState(_.merge({}, s, { withdrawal: { new: { amountStats: val } } }))
                break
            case 'Withdrawal New Frozen Stats':
                this.setState(_.merge({}, s, { withdrawal: { new: { frozenStats: val } } }))
                break
            case 'Withdrawal Accepted Transfer Stats':
                this.setState(_.merge({}, s, { withdrawal: { accepted: { transferStats: val } } }))
                break
            case 'Withdrawal Accepted Fee Stats':
                this.setState(_.merge({}, s, { withdrawal: { accepted: { feeStats: val } } }))
                break
            default:
                console.log('Unexpected Object', obj, val)
        }

    }

    render () {
        console.log(this.state.withdrawal)
        let ipState = this.state.innerProfit
        const innerProfit = Object
                .keys(ipState)
                .map(k => {
                    let v = ipState[k] / 100
                    return (<div>+{v.toFixed(2)}&nbsp;{k}</div>)
                })
        const pairedVals = (a, b) => {
            return Object.keys(a).map(k => {
                let i = 0, f = 0
                if(a.hasOwnProperty(k))
                    i = a[k] / 100
                if(b.hasOwnProperty(k))
                    f = b[k] / 100
                return(<div className={`${k.toLowerCase()}`}>
                    <span>{k}</span>
                    <span>: </span>
                    <span>+{i.toFixed(2)}</span>
                    <span> / </span>
                    <span>+{f.toFixed(2)}</span>
                </div>)})
        }
        const lbl = this.props.labels
        const wwl = this.state.withdrawal
        const dpt = this.state.deposit
        return(<React.Fragment>
        <div className="mb-3">
            <span>{`${this.props.labels.userCount}`}: </span>
            <span>{`${this.state.userCount}`}</span>
        </div>
        <div className="mb-3">
            <h2>{this.props.labels.fee.stats}</h2>
            <div>{`${this.props.labels.innerProfit}`}: </div>
            {innerProfit}
        </div>
        <div className="mb-3">
            <h2>{this.props.labels.deposit.stats}</h2>
            <div>
                <span>{`${this.props.labels.activeDeposits}`}: </span>
                <span>{this.state.activeDeposit.count}</span>
            </div>
            <div className="mb-2">
                <span>{`${this.props.labels.acceptedDeposits}`}: </span>
                <span>{this.state.acceptedDeposit.count}</span>
            </div>
            <div>
                <span>{`${this.props.labels.deposit.income.realTotal}`}</span>
                <span> / </span>
                <span>{`${this.props.labels.deposit.income.feeTotal}`}</span>
                <span>:</span>
            </div>
            {pairedVals(dpt.income.real, dpt.income.fee)}
        </div>
        <div className="mb-3">
            <h2>{this.props.labels.withdrawal.stats}</h2>
            <div>
                <span>{`${this.props.labels.withdrawal.new.count}`}: </span>
                <span>{this.state.withdrawal.new.count}</span>
            </div>
            <div className="mb-2">
                <div>{lbl.withdrawal.new.amount} / {lbl.withdrawal.new.frozen}</div>
                {pairedVals(wwl.new.amountStats, wwl.new.frozenStats)}
            </div>
            <div>
                <div>{lbl.withdrawal.accepted.count}: {wwl.accepted.count}</div>
                <div>{lbl.withdrawal.accepted.transfered} / {lbl.withdrawal.accepted.fee}</div>
                {pairedVals(wwl.accepted.transferStats, wwl.accepted.feeStats)}
            </div>
        </div>
        <div className="mb-2">
            <h2>
                {`${this.props.labels.wallets.stats}`}
            </h2>
            <div>{this.props.labels.wallets.balanceTotals}:</div>
            {Object.keys(this.state.wallets.total).map(k => {
                let v = this.state.wallets.total[k] / 100
                return(<div className={`${k.toLowerCase()}`}>
                    <span>{k}</span>
                    <span>: </span>
                    <span>+{v.toFixed(2)}</span>
                </div>)})
            }
        </div>

        </React.Fragment>)
    }
}