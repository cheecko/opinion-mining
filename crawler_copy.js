const axios = require('axios')
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

const positions = ['student', 'employee', 'manager', 'contractor'];

(async () => {
  const data = await Promise.all(companies.map(async company => {
		let page = 1
		let pagesCount = 3
		let data = {}

		while(page <= pagesCount) {
			const params = {
				'filters[position][]': 'student',
				reviewType: 'employees',
				urlParams: 'position%3Dstudent',
				page: page
			}
			const response = await axios.get(`https://www.kununu.com/middlewares/profiles/de/${company.name}/${company.uuid}/reviews`, { params: params })
			if(page === 1) {
				data = {
					common: response.data.common,
					page: response.data.page,
					pagesCount: response.data.pagesCount,
					perPage: response.data.perPage,
					recommendationRate: response.data.recommendationRate,
					score: response.data.score,
					scoreRanges: response.data.scoreRanges,
					totalReviews: response.data.totalReviews,
					reviews: []
				}
			}
			data.reviews = [...data.reviews, ...response.data.reviews] 

			page++
			// pagesCount = response.data.pagesCount
		}

		return data
  }))

  console.log(data)
	await fs.promises.writeFile('reviews.json', JSON.stringify(data, null, 2));
})();