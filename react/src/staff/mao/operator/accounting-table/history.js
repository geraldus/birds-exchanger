import React from 'react'


export default class History extends React.Component {
    render () {
        const vs = this.props.values
        return (
        <table className="table table-hover">
            <thead>
                <tr>
                    <th>Дата</th>
                    <th>ФИО</th>
                    <th>Завёл ₽</th>
                    <th>Курс</th>
                    <th>Получил ₽</th>
                    <th>Курс</th>
                    <th>Завёл PZM</th>
                    <th>Вывел PZM</th>
                    <th>Примечание</th>
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
                        <td>{v[6]}</td>
                        <td>{v[7]}</td>
                        <td>{v[8]}</td>
                        <td>{v[9]}</td>
                    </tr>)
                })}
            </tbody> }
        </table>
        )
    }
}