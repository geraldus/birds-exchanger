import React from 'react'


export default class Totals extends React.Component {
    render () {
        const vs = this.props.values
        return (
        <table className="table table-hover">
            <thead>
                <tr>
                    <th>ФИО</th>
                    <th>Завёл PZM</th>
                    <th>Вывел PZM</th>
                    <th>Остаток</th>
                    <th>Парамайнинг</th>
                    <th>Кошелёк</th>
                </tr>
            </thead>
            {vs.length > 0 && <tbody>
                {vs.map((x, i) => {
                    const v = x.value
                    return (<tr className={x.updated? 'update' : ''}>
                        <td>{v[0]}</td>
                        <td>{v[1]}</td>
                        <td>{v[2]}</td>
                        <td>{v[3]}</td>
                        <td>{v[4]}</td>
                        <td>{v[5]}</td>
                    </tr>)
                })}
            </tbody> }
        </table>
        )
    }
}