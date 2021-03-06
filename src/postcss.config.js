const purgecss = require('@fullhuman/postcss-purgecss')({

  // Specify the paths to all of the template files in your project 
  content: [
    './src/**/*.html',
    './src/**/*.elm',
    // etc.
  ],

  // Include any special characters you're using in this regular expression
  defaultExtractor: content => content.match(/[\w-/:]+(?<!:)/g) || []
})


module.exports = ({ options }) => {
  return {
    plugins: [
    require('tailwindcss'),
    options.mode === 'production' ? purgecss : undefined,
    require('autoprefixer')({browsers: ['last 2 versions', 'iOS >= 8']}),
    ]
  }
}