# SMA-organization-simulation

O presente trabalho insere-se na disciplina de Sistemas Multi-Agente e Simulação de Organizações, e tem como objetivo a utilização de um programa Agent.micro.econ (função simplificada de 2012), implementado em linguagem R, que simula microeconomia com agentes. A microeconomia que também é denominada “teoria dos preços”, em raras ocasiões, proporciona uma teoria acerca dos preços que surgiram efetivamente em determinadas situações. Nos mercados concorrenciais, a microeconomia não proporciona uma definição do preço que realmente surgirá, mas proporciona uma teoria da existência de um preço de equilíbrio. Nestes mercados, tanto compradores como vendedores são “tomadores de preços”, não podendo influenciar. Porém, como se determina o preço? Para equacionar este impasse, a teoria microeconômica recorre a um agente fictício: o leiloeiro.

O mercado caracteriza um conjunto de agentes que pertencem a alguns sectores de atividade. Em determinadas experiências utilizámos dados em que os sectores representados são o sector Agrícola, de Vestuário, de Transportes e de Combustíveise noutras experiências,utilizámos dados em que se representa os sectores Agrícola, de Vestuário, de Transportes, de Saúdee Monetário. 

Este programa pretende simular alguns comportamentos de mercado e os efeitos da produção e do consumo nesse mercado, simulando as ações entre os agentes e o leiloeiro, sendo que os primeiros comunicam ao leiloeiro as quantidades procuradas por cada agente, por cada bem e este por sua vez, comunica os preços, assim sucessivamente.

Assume-se que todos os agentesde cada sector já têm alguns bens no início e são também dados os preços iniciais de cada um destes bens. Além disso, é dada a função de utilidade e os valores de utilidade para os bens decada sector. Estes valores permitem a cada agente calcular a sua procura, isto é, as quantidades de bens a vender ou adquirir no mercado. 

O mercado contém ainda o leiloeiro, que interage com os agentes. Estes comunicam a procura ao leiloeiro, que por sua vez estabelece o preço de cada bem. Este processo pode envolver várias iterações, mas quando os preços estabilizam, isto é, quando for atingido um equilíbrio entre a oferta e a procura, o processo termina. A seguir efetuam-seas trocas (compra ou venda), sendo estas feitas uma vez por semana.
Assim, neste trabalho iremos variar certos parâmetros dafunção setup()(dados iniciais do mercado) para avaliar o efeito destas variações sobre o mesmo, assim como fazer alterações ao programa Agent.micro.econ por forma a responder a alguns objetivos propostos.

for complete report please check following links:
(Para relatório completo siga os links):

https://www.researchgate.net/publication/325810463_SISTEMAS_MULTI-AGENTE_E_SIMULACAO_DE_ORGANIZACOES

https://www.researchgate.net/publication/325810404_SISTEMAS_MULTI-AGENTE_E_SIMULACAO_DE_ORGANIZACOES_-_parte_2


