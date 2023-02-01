const fs = require('fs')

const companies = [
	{
		name: 'deutschebahn',
		uuid: '4cd02b99-e78a-4523-a1ca-453cd3fb5ab9'
	},
	// {
	// 	name: 'deutsche-telekom',
	// 	uuid: 'ea360420-065c-4ca1-867b-4b177ce8224c'
	// }
];

// 'student', 'employee', 'manager', 'contractor'
const positions = ['student', 'employee', 'manager', 'contractor'];

const createCSV = (json) => {
	if (json.length == 0) return

	const headers = Object.keys(json[0])
	const csv = [
		headers.join(','), // header row first
		...json.map(row => headers.map(header => JSON.stringify(row[header] === null || !row[header]  ? '' : row[header])).join(','))
	].join('\r\n')

	return csv
}

(async () => {
  companies.forEach(async company => {
    positions.forEach(async position => {
      const texts = await fs.promises.readFile(`exports/${company.name}_${position}.json`)
      const json = JSON.parse(texts);
      const csv = createCSV(json)
      await fs.promises.writeFile(`exports/${company.name}_${position}.csv`, csv);
    })
  })
})();