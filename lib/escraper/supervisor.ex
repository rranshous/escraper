defmodule Escraper.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init(args) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(Escraper.Worker, [])
      worker(Escraper.PageCache, []),
      worker(Escraper.WorkQueue, []),
      worker(Escraper.SiteScraper, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
