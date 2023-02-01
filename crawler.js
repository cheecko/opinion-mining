// can do it all at the same time, but better do it one b one because of the captcha
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

// 'student', 'employee', 'manager', 'contractor'
const positions = ['manager'];

(async () => {
  const data = await Promise.all(companies.map(async company => {
		return await Promise.all(positions.map(async position => {
			let page = 1
			let pagesCount = 100
			let data = {}
			let reviews = []
	
			while(page <= pagesCount) {
				const params = {
					'filters[position][]': position,
					reviewType: 'employees',
					urlParams: `position%3D${position}`,
					page: page
				}
				const response = await axios.get(`https://www.kununu.com/middlewares/profiles/de/${company.name}/${company.uuid}/reviews`, { params: params })
				// if(page === 1) {
				// 	data = {
				// 		common: response.data.common,
				// 		page: response.data.page,
				// 		pagesCount: response.data.pagesCount,
				// 		perPage: response.data.perPage,
				// 		recommendationRate: response.data.recommendationRate,
				// 		score: response.data.score,
				// 		scoreRanges: response.data.scoreRanges,
				// 		totalReviews: response.data.totalReviews,
				// 		reviews: []
				// 	}
				// }
				// data.reviews = [...data.reviews, ...response.data.reviews] 

				reviews = [...reviews, ...response?.data?.reviews.map(review => {
					return {
						company: response?.data?.common?.slug,
						position: response?.data?.common?.filters?.position[0],
						companyLocation: `${review?.company?.location?.city}, ${review?.company?.location?.state}`,
						employeePosition: review?.position,
						employeeDepartment: review?.department,
						title: review?.title,
						score: review?.score,
						createdAt: review?.createdAt,
						positive: review?.texts.find(text => text.id === 'positive')?.text ?? '',
						negative: review?.texts.find(text => text.id === 'negative')?.text ?? '',
						suggestion: review?.texts.find(text => text.id === 'suggestion')?.text ?? '',
						atmosphere: review?.ratings.find(text => text.id === 'atmosphere')?.text ?? '',
						image: review?.ratings.find(text => text.id === 'image')?.text ?? '',
						workLife: review?.ratings.find(text => text.id === 'workLife')?.text ?? '',
						career: review?.ratings.find(text => text.id === 'career')?.text ?? '',
						salary: review?.ratings.find(text => text.id === 'salary')?.text ?? '',
						environment: review?.ratings.find(text => text.id === 'environment')?.text ?? '',
						teamwork: review?.ratings.find(text => text.id === 'teamwork')?.text ?? '',
						oldColleagues: review?.ratings.find(text => text.id === 'oldColleagues')?.text ?? '',
						leadership: review?.ratings.find(text => text.id === 'leadership')?.text ?? '',
						workConditions: review?.ratings.find(text => text.id === 'workConditions')?.text ?? '',
						communication: review?.ratings.find(text => text.id === 'communication')?.text ?? '',
						equality: review?.ratings.find(text => text.id === 'equality')?.text ?? '',
						tasks: review?.ratings.find(text => text.id === 'tasks')?.text ?? '',
					}
				})]
				page++
				// pagesCount = response?.data?.pagesCount
			}
			console.log(reviews)
			await fs.promises.writeFile(`exports/${company.name}_${position}.json`, JSON.stringify(reviews, null, 2));
	
			return data
		}))
		
  }))

  console.log(data)
	await fs.promises.writeFile('exports/reviews.json', JSON.stringify(data, null, 2));

	const companiesOverall = data.map(company => {
		return company.map(position => {
			return {
				company: position?.common?.slug,
				position: position?.common?.filters?.position[0],
				recommendationPercentage: position?.recommendationRate?.percentage,
				recommendationTotalReviews: position?.recommendationRate?.totalReviews,
				recommendedTotalReviews: position?.recommendationRate?.recommendedTotalReviews,
				notRecommendedTotalReviews: position?.recommendationRate?.notRecommendedTotalReviews,
				totalScore: position?.score,
				totalReviews: position?.totalReviews
			}
		})
	})
	console.log(companiesOverall)
	await fs.promises.writeFile('exports/companiesOverall.json', JSON.stringify(companiesOverall, null, 2));
})();