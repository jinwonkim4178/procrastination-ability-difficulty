<h1 align="left">Course Difficulty and Ability Moderate the Impact of Procrastination on Grades </h1>
<h2>Introduction</h2>
<p>
This repository provides the preprocessing (preprocessing.R) and analysis (analysis.R) procedures for the paper 
<i>"Course Difficulty and Ability Moderate the Impact of Procrastination on Grades"</i>. 
</p>

<h2>Data Preprocessing</h2>
<p>The preprocessing pipeline includes:</p>
<ul>
  <li><b>Compute procrastination index</b>: Constructed a rank-based index of submission delays within each course–term.</li>
  <li><b>Filter by grading scheme</b>: Excluded courses graded on a Pass/Fail basis and enrollments without final letter grades (A–F).</li>
  <li><b>Restrict to sufficiently large majors</b>
    <ul>
      <li>≥ 20 distinct courses</li>
      <li>≥ 100 enrollments</li>
      <li>Top 10% by average enrollment per course</li>
    </ul>
  </li>
  <li><b>Drop failed IRT* cases</b>: Removed majors where IRT estimation failed due to data sparsity.</li>
  <li><b>Remove multidimensional majors</b>: Dropped majors showing evidence of multidimensionality.</li>
</ul>
<p>
*We performed the IRT estimation following the procedure provided in 
<a href="https://dl.acm.org/doi/abs/10.1145/3657604.3662028">Baucks et al. (2024)</a>, 
whose replication code is publicly available on 
<a href="https://github.com/your-repo-link">GitHub</a>.
</p>

<h2>Data Analysis</h2>
<p>The analysis builds on the final dataset and proceeds in the following steps:</p>
<ul>
  <li><b>Standardization</b><br>
      All three explanatory variables (procrastination index, course difficulty, and student ability) were standardized to have mean 0 and standard deviation 1.
  </li>

  <li><b>Hierarchical Linear Models (HLMs)</b><br>
      Six nested models were estimated using <code>lmer</code>:
      <ol>
        <li>Course grade ~ Procrastination + (1 | Student)</li>
        <li>Course grade ~ Procrastination + Course difficulty + (1 | Student)</li>
        <li>Course grade ~ Procrastination + Course difficulty + Student ability + (1 | Student)</li>
        <li>Course grade ~ Procrastination × Course difficulty + Student ability + (1 | Student)</li>
        <li>Course grade ~ Procrastination × (Course difficulty + Student ability) + (1 | Student)</li>
        <li>Course grade ~ Procrastination × Course difficulty × Student ability + (1 | Student)</li>
      </ol>
  </li>

  <li><b>Model diagnostics</b><br>
      Residual diagnostics were performed, including Q–Q plots, residuals vs. fitted plots, scale–location plots, and leverage/outlier checks.
  </li>

  <li><b>Calculating Robust standard errors</b><br>
      All reported results were based on robust standard errors (cluster-robust variance estimators).
  </li>

  <li><b>Model comparison</b><br>
      AIC, BIC, and Likelihood ratio tests (<code>anova</code>) were used to compare nested models.
  </li>

  <li><b>Three-way interaction plot</b><br>
      Visualization of the interaction between procrastination, student ability, and course difficulty on course grades. 
  </li>
</ul>
